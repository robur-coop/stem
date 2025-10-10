module Stream = struct
  type 'a t = {
    buffer : 'a option array;
    mutable rd_pos : int;
    mutable wr_pos : int;
    lock : Miou.Mutex.t;
    non_empty : Miou.Condition.t;
    non_full : Miou.Condition.t;
    mutable closed : bool;
  }

  let create size =
    let lock = Miou.Mutex.create () in
    let non_empty = Miou.Condition.create () in
    let non_full = Miou.Condition.create () in
    {
      buffer = Array.make size None;
      lock;
      rd_pos = 0;
      wr_pos = 0;
      non_empty;
      non_full;
      closed = false;
    }

  let put t data =
    Miou.Mutex.protect t.lock @@ fun () ->
    if t.closed then invalid_arg "Stream.put closed stream" ;
    while (t.wr_pos + 1) mod Array.length t.buffer = t.rd_pos do
      Miou.Condition.wait t.non_full t.lock
    done ;
    t.buffer.(t.wr_pos) <- Some data ;
    t.wr_pos <- (t.wr_pos + 1) mod Array.length t.buffer ;
    Miou.Condition.signal t.non_empty

  let get t =
    Miou.Mutex.protect t.lock @@ fun () ->
    while t.wr_pos = t.rd_pos && not t.closed do
      Miou.Condition.wait t.non_empty t.lock
    done ;
    if t.closed && t.wr_pos = t.rd_pos
    then None
    else
      let data = t.buffer.(t.rd_pos) in
      t.buffer.(t.rd_pos) <- None ;
      t.rd_pos <- (t.rd_pos + 1) mod Array.length t.buffer ;
      Miou.Condition.signal t.non_full ;
      data

  let close t =
    Miou.Mutex.protect t.lock @@ fun () ->
    t.closed <- true ;
    Miou.Condition.signal t.non_empty

  let of_seq ?(parallel = false) size seq =
    let t = create size in
    let fn () =
      Seq.iter (put t) seq ;
      close t in
    if parallel then (t, Miou.call fn) else (t, Miou.async fn)
end

let ( let@ ) finally fn = Fun.protect ~finally fn
let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

let none_if_stop language =
  let stops = List.assoc language Stopwords.words in
  fun word -> if List.mem word stops then None else Some word

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
    | Some orig, Some stem, Some (count, srcs) ->
        let srcs = if List.mem orig srcs then srcs else orig :: srcs in
        Hashtbl.replace dataset stem (count + 1, srcs) ;
        go ()
    | Some orig, Some stem, None ->
        Hashtbl.add dataset stem (1, [ orig ]) ;
        go ()
    | Some _, None, _ -> go ()
    | None, _, _ -> dataset in
  go ()

let run _ separator encoding language actions filename =
  Miou_unix.run @@ fun () ->
  let ic = open_in filename in
  let@ () = fun () -> close_in ic in
  let len = in_channel_length ic in
  let buf = Bytes.create len in
  really_input ic buf 0 len ;
  let str = Bytes.unsafe_to_string buf in
  let str = Seq.return str in
  let words = Tokenizer.run actions str in
  let queue, prm0 = Stream.of_seq ~parallel:true 0x100 words in
  let prm1 = Miou.call (counter encoding language queue) in
  Miou.await_exn prm0 ;
  let dataset = Miou.await_exn prm1 in
  let seq = Hashtbl.to_seq dataset in
  let lst = List.of_seq seq in
  let lst = List.sort (fun (_, (a, _)) (_, (b, _)) -> Int.compare b a) lst in
  let fn (stem, (count, _)) = Fmt.pr "%S%s%d\n%!" stem separator count in
  List.iter fn lst

open Cmdliner

let input =
  let parser str =
    if str = "-"
    then Ok str
    else if Sys.file_exists str && Sys.is_directory str = false
    then Ok str
    else error_msgf "%s does not exist (or %s is not a file)" str str in
  Arg.conv (parser, Fmt.string)

let input =
  let open Arg in
  required & pos ~rev:true 0 (some input) None & info [] ~docv:"FILE"

let separator =
  let doc = "The separator between stem words and frequencies." in
  let is_printable = function '\x21' .. '\x7e' -> true | _ -> false in
  let parser str =
    if String.length str <> 1 || not (is_printable str.[0])
    then error_msgf "Invalid separator %S" str
    else Ok str in
  let separator = Arg.conv (parser, Fmt.string) in
  let open Arg in
  value & opt separator "," & info [ "s"; "separator" ] ~doc ~docv:"SEPARATOR"

let term =
  let open Term in
  const run
  $ Stem_cli.setup_logs
  $ separator
  $ Stem_cli.encoding
  $ Stem_cli.language
  $ Stem_cli.actions
  $ input

let cmd =
  let doc = "Stem words." in
  let man = [] in
  let info = Cmd.info "stemmer" ~doc ~man in
  Cmd.v info term

let () = Cmd.(exit @@ eval cmd)
