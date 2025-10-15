let run _ encoding language actions documents query =
  Miou_unix.run @@ fun () ->
  let cfg = Bm25.config ~encoding ~actions language in
  let t = Bm25.make ~cfg documents in
  let results = Bm25.rank t query in
  let results = List.sort (fun (_, a) (_, b) -> Float.compare b a) results in
  let fn (filename, score) = Fmt.pr "%s: %f\n%!" filename score in
  List.iter fn results

let ( let@ ) finally fn = Fun.protect ~finally fn
let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

open Cmdliner

let setup_documents documents collections =
  let rec go ic acc =
    match input_line ic with
    | str ->
        if Sys.file_exists str && not (Sys.is_directory str)
        then go ic (str :: acc)
        else go ic acc
    | exception End_of_file -> List.rev acc in
  let stdin = ref false in
  let rec on_collections acc = function
    | [] -> acc
    | "-" :: collections when not !stdin ->
        stdin := true ;
        on_collections (go Stdlib.stdin acc) collections
    | "-" :: collections -> on_collections acc collections
    | str :: collections ->
        let acc : string list =
          if Sys.file_exists str && not (Sys.is_directory str)
          then
            let ic = open_in str in
            let@ () = fun () -> close_in ic in
            go ic acc
          else acc in
        on_collections acc collections in
  on_collections documents collections

let documents =
  let doc = "A document in which we have to search." in
  let parser str =
    if Sys.file_exists str && not (Sys.is_directory str)
    then Ok str
    else error_msgf "%S does not exist (or is not a regular file)" str in
  let existing_document = Arg.conv (parser, Fmt.string) in
  let open Arg in
  value
  & opt_all existing_document []
  & info [ "d"; "document" ] ~doc ~docv:"FILE"

let collections =
  let doc =
    "A collection of documents in which we have to search (it can be \
     $(i,stdin) if the user give $(i,-))." in
  let parser = function
    | "-" -> Ok "-"
    | str ->
        if Sys.file_exists str && not (Sys.is_directory str)
        then Ok str
        else error_msgf "%S does not exist (or is not a regular file)" str in
  let existing_collection = Arg.conv (parser, Fmt.string) in
  let open Arg in
  value
  & opt_all existing_collection []
  & info [ "c"; "collection" ] ~doc ~docv:"FILE"

let setup_documents =
  let open Term in
  const setup_documents $ documents $ collections

let query =
  let doc = "The search query." in
  let open Arg in
  required & pos 0 (some string) None & info [] ~doc ~docv:"QUERY"

let term =
  let open Term in
  const run
  $ Stem_cli.setup_logs
  $ Stem_cli.encoding
  $ Stem_cli.language
  $ Stem_cli.actions
  $ setup_documents
  $ query

let cmd =
  let doc = "Score the documents provided according to a search query." in
  let man = [] in
  let info = Cmd.info "search" ~doc ~man in
  Cmd.v info term

let () = Cmd.(exit @@ eval cmd)
