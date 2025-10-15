module S = S
module Norm = Norm

type behavior = Norm.behavior =
  | Remove
  | Isolate
  | Merge_with_previous
  | Merge_with_next

type pattern = Whitespace | Dash | Bert | Regex of Re.re
type action = pattern * Norm.behavior

let from_regex re : (module S.PATTERN) =
  let module M = struct
    let find_matches ?encoding:_ str =
      let fn = function
        | `Delim g -> { S.str = Re.Group.get g 0; is_match = true }
        | `Text str -> { S.str; is_match = false } in
      Re.Seq.split_full re str |> Seq.map fn

    let find_matches_on_bstr ?encoding bstr =
      find_matches ?encoding (Bstr.to_string bstr)
  end in
  (module M)

let pattern_to_module pattern : (module S.PATTERN) =
  match pattern with
  | Whitespace -> (module Whitespace)
  | Dash -> (module Dash)
  | Bert -> (module Bert)
  | Regex re -> from_regex re

let to_lowercase_utf_8 str =
  let buf = Buffer.create 0x100 in
  let fold () _pos = function
    | `Malformed str -> Buffer.add_string buf str
    | `Uchar uchr ->
    match Uucp.Case.Map.to_lower uchr with
    | `Self -> Uutf.Buffer.add_utf_8 buf uchr
    | `Uchars lst -> List.iter (Uutf.Buffer.add_utf_8 buf) lst in
  Uutf.String.fold_utf_8 fold () str ;
  Buffer.contents buf

let run ?(encoding = Snowball.UTF_8) ?(to_lowercase = true) lst seq =
  let fn (acc : string Seq.t) (pattern, behavior) =
    let pattern = pattern_to_module pattern in
    let fn = Norm.split ~encoding ~pattern ~behavior in
    Seq.flat_map fn acc in
  let seq = Seq.fold_left fn seq (List.to_seq lst) in
  match (encoding, to_lowercase) with
  | _, false -> seq
  | Snowball.UTF_8, true -> Seq.map to_lowercase_utf_8 seq
  | _ -> seq

let run_on_bstr ?(encoding = Snowball.UTF_8) ?to_lowercase lst bstr =
  match lst with
  | (pattern, behavior) :: lst ->
      let pattern = pattern_to_module pattern in
      let seq = Norm.split_on_bstr ~encoding ~pattern ~behavior bstr in
      run ~encoding ?to_lowercase lst seq
  | [] -> Seq.return (Bstr.to_string bstr)
