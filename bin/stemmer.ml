let ( let@ ) finally fn = Fun.protect ~finally fn
let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

let run encoding language input =
  let ic, finally =
    match input with
    | "-" -> (stdin, ignore)
    | input ->
        let ic = open_in input in
        let finally () = close_in ic in
        (ic, finally) in
  let@ () = finally in
  let t = Snowball.create ~encoding language in
  let@ () = fun () -> Snowball.remove t in
  let rec go () =
    match input_line ic with
    | word ->
        let stem = Snowball.stem t word in
        print_endline stem ;
        go ()
    | exception End_of_file -> () in
  go ()

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

let term =
  let open Term in
  const run $ Stem_cli.encoding $ Stem_cli.language $ input

let cmd =
  let doc = "Stem words." in
  let man = [] in
  let info = Cmd.info "stemmer" ~doc ~man in
  Cmd.v info term

let () = Cmd.(exit @@ eval cmd)
