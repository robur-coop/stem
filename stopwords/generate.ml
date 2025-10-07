let files =
  [
    "arabic.txt";
    "danish.txt";
    "dutch.txt";
    "english.txt";
    "finnish.txt";
    "french.txt";
    "german.txt";
    "hungarian.txt";
    "indonesian.txt";
    "italian.txt";
    "norwegian.txt";
    "portuguese.txt";
    "russian.txt";
    "spanish.txt";
    "swedish.txt";
  ]

module S = Set.Make (String)

let dictionary_from_file filename =
  let ic = open_in filename in
  let finally () = close_in ic in
  Fun.protect ~finally @@ fun () ->
  let rec go m =
    match input_line ic with
    | exception End_of_file -> m
    | line ->
        let[@warning "-8"] (str :: _) = String.split_on_char '|' line in
        let str = String.trim str in
        if String.length str > 0 then go (S.add str m) else go m in
  go S.empty

let dictionary_to_ocaml_module m filename =
  let oc = open_out filename in
  let finally () = close_out oc in
  Fun.protect ~finally @@ fun () ->
  let ppf = Format.formatter_of_out_channel oc in
  let lst = S.to_list m in
  Fmt.pf ppf "let words = @[<hov>%a@] ;;" Fmt.(Dump.list (fmt "%S")) lst

let () =
  let fn filename =
    let basename = Filename.chop_extension filename in
    let m = dictionary_from_file filename in
    dictionary_to_ocaml_module m (basename ^ ".ml") in
  List.iter fn files
