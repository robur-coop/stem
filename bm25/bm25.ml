type 'uid document = {
  uid : 'uid;
  tokens : (string, int) Hashtbl.t;
  length : int;
}

type config = {
  language : Snowball.Language.t;
  encoding : Snowball.encoding;
  actions : Tokenizer.action list;
  parallel : bool;
}

let default_actions = Tokenizer.[ (Whitespace, Remove); (Bert, Isolate) ]

let config ?(parallel = true) ?(encoding = Snowball.UTF_8)
    ?(actions = default_actions) language =
  { language; parallel; encoding; actions }

let ( let@ ) finally fn = Fun.protect ~finally fn

let none_if_stop language =
  match List.assoc_opt language Stopwords.words with
  | Some stops -> fun word -> if List.mem word stops then None else Some word
  | None -> Option.some

let counter encoding language queue () =
  let dataset = Hashtbl.create 0x100 in
  let stemmer = Snowball.create ~encoding language in
  let@ () = fun () -> Snowball.remove stemmer in
  let none_if_stop = none_if_stop language in
  let rec go () =
    let orig = Stream.get queue in
    let stem = Option.map (Snowball.stem stemmer) orig in
    let stem = Option.bind stem none_if_stop in
    let count = Option.bind stem (Hashtbl.find_opt dataset) in
    match (orig, stem, count) with
    | Some _orig, Some stem, Some count ->
        Hashtbl.replace dataset stem (count + 1) ;
        go ()
    | Some _orig, Some stem, None ->
        Hashtbl.add dataset stem 1 ;
        go ()
    | Some _, None, _ -> go ()
    | None, _, _ -> dataset in
  go ()

type 'uid file = 'uid * [ `Contents of Bstr.t | `File of string ]

let freqs_of_document ~cfg (uid, contents) =
  let contents =
    match contents with
    | `Contents bstr -> `Bigstring bstr
    | `File filename ->
        let ic = open_in filename in
        let@ () = fun () -> close_in ic in
        let len = in_channel_length ic in
        let buf = Bytes.create len in
        really_input ic buf 0 len ;
        `String (Bytes.unsafe_to_string buf) in
  let words =
    match contents with
    | `Bigstring bstr -> Tokenizer.run_on_bstr cfg.actions bstr
    | `String str -> Tokenizer.run cfg.actions (Seq.return str) in
  let queue, prm0 = Stream.of_seq ~parallel:cfg.parallel 0x100 words in
  let prm1 =
    if cfg.parallel
    then Miou.call (counter cfg.encoding cfg.language queue)
    else Miou.async (counter cfg.encoding cfg.language queue) in
  Miou.await_exn prm0 ;
  let tokens = Miou.await_exn prm1 in
  let length = Hashtbl.length tokens in
  { uid; tokens; length }

let freqs_of_document ~cfg filename =
  try freqs_of_document ~cfg filename
  with exn ->
    let bt = Printexc.get_raw_backtrace () in
    Fmt.epr "%s%!" (Printexc.raw_backtrace_to_string bt) ;
    raise exn

let or_raise = function Ok value -> value | Error exn -> raise exn

type 'uid t = {
  cfg : config;
  idf : (string, float) Hashtbl.t;
  avgdl : float;
  k1 : float;
  b : float;
  docs : 'uid document list;
}

let make ~cfg ?(k1 = 1.5) ?(b = 0.75) documents =
  let _N = Float.of_int (List.length documents) in
  let _total_length = ref 0 in
  let fn filename = freqs_of_document ~cfg filename in
  let docs = Miou.parallel fn documents in
  let docs = List.map or_raise docs in
  let total_length =
    let fn acc { length; _ } = acc + length in
    List.fold_left fn 0 docs |> Float.of_int in
  let df (* document frequency *) =
    let df = Hashtbl.create 0x100 in
    let fn { tokens; _ } =
      let fn token _ =
        match Hashtbl.find_opt df token with
        | Some freq -> Hashtbl.replace df token (freq + 1)
        | None -> Hashtbl.add df token 1 in
      Hashtbl.iter fn tokens in
    List.iter fn docs ;
    df in
  let avgdl = total_length /. _N in
  let idf = Hashtbl.create 0x100 in
  let fn token freq =
    let freq = Float.of_int freq in
    let value = Float.(log (1. +. ((_N -. freq +. 0.5) /. (freq +. 0.5)))) in
    Hashtbl.add idf token value in
  Hashtbl.iter fn df ;
  { cfg; idf; avgdl; k1; b; docs }

let tokenize_and_stem t query =
  let encoding = t.cfg.encoding in
  let actions = t.cfg.actions in
  let tokens = Tokenizer.run ~encoding actions (Seq.return query) in
  let stemmer = Snowball.create ~encoding t.cfg.language in
  let none_if_stop = none_if_stop t.cfg.language in
  let@ () = fun () -> Snowball.remove stemmer in
  let fn = Fun.compose none_if_stop (Snowball.stem stemmer) in
  let tokens = Seq.filter_map fn tokens in
  List.of_seq tokens

let score t query doc =
  let fn acc token =
    match Hashtbl.find_opt doc.tokens token with
    | None -> acc
    | Some freq ->
        let freq = Float.of_int freq in
        let idf = Hashtbl.find t.idf token in
        let _D = Float.of_int doc.length in
        let _n = freq *. (t.k1 +. 1.) in
        let _m = freq +. (t.k1 *. (1. -. t.b +. (t.b *. _D /. t.avgdl))) in
        acc +. (idf *. (_n /. _m)) in
  (doc.uid, List.fold_left fn 0.0 query)

let rank t query =
  let query = tokenize_and_stem t query in
  let fn = score t query in
  List.map fn t.docs
