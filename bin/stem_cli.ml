let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

open Cmdliner

let language_from_string str =
  let algs = Snowball.languages in
  let str = String.lowercase_ascii str in
  let fn (alg : Snowball.Language.t) = String.equal str (alg :> string) in
  match List.find_opt fn algs with
  | Some alg -> Ok alg
  | None -> error_msgf "Language %S not found" str

let tokenizer_from_string str =
  match String.lowercase_ascii str with
  | "whitespace" -> Ok Tokenizer.Whitespace
  | "dash" -> Ok Dash
  | "bert" -> Ok Bert
  | _ -> error_msgf "Invalid tokenizer: %S" str

let behavior_from_string str =
  match String.lowercase_ascii str with
  | "remove" -> Ok Tokenizer.Remove
  | "isolate" -> Ok Isolate
  | "merge_with_previous" | "merge-with-previous" -> Ok Merge_with_previous
  | "merge_with_next" | "merge-with-next" -> Ok Merge_with_next
  | _ -> error_msgf "Invalid behavior: %S" str

let re_from_string str =
  try Ok (Re.Pcre.regexp str)
  with _ -> error_msgf "Invalid regular expression: %S" str

let pp_tokenizer ppf = function
  | Tokenizer.Whitespace -> Fmt.string ppf "whitespace"
  | Dash -> Fmt.string ppf "dash"
  | Bert -> Fmt.string ppf "bert"
  | Regex re -> Fmt.pf ppf "re:%a" Re.pp_re re

let pp_behavior ppf = function
  | Tokenizer.Remove -> Fmt.string ppf "remove"
  | Isolate -> Fmt.string ppf "isolate"
  | Merge_with_previous -> Fmt.string ppf "merge-with-previous"
  | Merge_with_next -> Fmt.string ppf "merge-with-next"

let language =
  let algs = Snowball.languages in
  let pp ppf (alg : Snowball.Language.t) = Fmt.string ppf (alg :> string) in
  let language = Arg.conv (language_from_string, pp) in
  let doc =
    let algs =
      List.map (fun (alg : Snowball.Language.t) -> (alg :> string)) algs in
    let hd, tl = (List.hd algs, List.tl algs) in
    let tl = List.rev tl in
    Fmt.str
      "The language to process. $(tname) is able to handle these languages: %s \
       and %s."
      (String.concat ", " tl) hd in
  let open Arg in
  value
  & opt language Snowball.porter
  & info [ "l"; "language" ] ~doc ~docv:"LANGUAGE"

let encoding =
  let open Arg in
  let docs = "ENCODINGS" in
  let encodings =
    [
      (Snowball.UTF_8, info [ "utf-8" ] ~doc:"UTF-8 encoding" ~docs);
      ( Snowball.ISO_8859_1,
        info [ "iso-8859-1"; "latin1" ] ~doc:"Latin1 encoding" ~docs );
      ( Snowball.ISO_8859_2,
        info [ "iso-8859-2"; "latin2" ] ~doc:"Latin2 encoding" ~docs );
      (Snowball.KOI8_R, info [ "koi8-r" ] ~doc:"KOI8-R encoding" ~docs);
    ] in
  value & vflag Snowball.UTF_8 encodings

let action =
  let ( let* ) = Result.bind in
  let parser str =
    match String.split_on_char ':' str with
    | [ tokenizer ] ->
        let* tokenizer = tokenizer_from_string tokenizer in
        Ok (tokenizer, Tokenizer.Remove)
    | ("re" | "RE" | "rE" | "Re") :: behavior :: re ->
        let* behavior = behavior_from_string behavior in
        let* re = re_from_string (String.concat ":" re) in
        Ok (Tokenizer.Regex re, behavior)
    | tokenizer :: behavior ->
        let behavior = String.concat ":" behavior in
        let* tokenizer = tokenizer_from_string tokenizer in
        let* behavior = behavior_from_string behavior in
        Ok (tokenizer, behavior)
    | [] -> assert false in
  let pp ppf (tokenizer, action) =
    match tokenizer with
    | Tokenizer.Regex re -> Fmt.pf ppf "re:%a:%a" pp_behavior action Re.pp_re re
    | tokenizer -> Fmt.pf ppf "%a:%a" pp_tokenizer tokenizer pp_behavior action
  in
  Arg.conv (parser, pp)

let actions =
  let doc = "An action to $(i,tokenize) the given document and split words." in
  let open Arg in
  value
  & opt_all action [ Tokenizer.(Whitespace, Remove) ]
  & info [ "a"; "action" ] ~doc ~docv:"ACTION"

let () = Logs_threaded.enable ()
let output_options = "OUTPUT OPTIONS"

let verbosity =
  let env = Cmd.Env.info "BLAZE_LOGS" in
  Logs_cli.level ~docs:output_options ~env ()

let renderer =
  let env = Cmd.Env.info "BLAZE_FMT" in
  Fmt_cli.style_renderer ~docs:output_options ~env ()

let utf_8 =
  let doc = "Allow binaries to emit UTF-8 characters." in
  let env = Cmd.Env.info "BLAZE_UTF_8" in
  Arg.(value & opt bool true & info [ "with-utf-8" ] ~doc ~env)

let reporter ppf =
  let report src level ~over k msgf =
    let k _ =
      over () ;
      k () in
    let with_metadata header _tags k ppf fmt =
      Fmt.kpf k ppf
        ("[%a]%a[%a]: " ^^ fmt ^^ "\n%!")
        Fmt.(styled `Cyan int)
        (Stdlib.Domain.self () :> int)
        Logs_fmt.pp_header (level, header)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src) in
    msgf @@ fun ?header ?tags fmt -> with_metadata header tags k ppf fmt in
  { Logs.report }

let setup_logs utf_8 style_renderer level =
  Fmt_tty.setup_std_outputs ~utf_8 ?style_renderer () ;
  Logs.set_level level ;
  Logs.set_reporter (reporter Fmt.stderr) ;
  Option.is_none level

let setup_logs = Term.(const setup_logs $ utf_8 $ renderer $ verbosity)
