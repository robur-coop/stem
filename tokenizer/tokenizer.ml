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
  end in
  (module M)

let pattern_to_module pattern : (module S.PATTERN) =
  match pattern with
  | Whitespace -> (module Whitespace)
  | Dash -> (module Dash)
  | Bert -> (module Bert)
  | Regex re -> from_regex re

let run lst seq =
  let fn (acc : string Seq.t) (pattern, behavior) =
    let pattern = pattern_to_module pattern in
    let fn = Norm.split ~pattern ~behavior in
    Seq.flat_map fn acc in
  Seq.fold_left fn seq (List.to_seq lst)
