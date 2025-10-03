module S = S
module Whitespace = Whitespace
module Dash = Dash
module Norm = Norm

type behavior = Norm.behavior =
  | Remove
  | Isolate
  | Merge_with_previous
  | Merge_with_next

type pattern = Whitespace | Dash
type action = pattern * Norm.behavior

let pattern_to_module pattern : (module S.PATTERN) =
  match pattern with Whitespace -> (module Whitespace) | Dash -> (module Dash)

let run lst str =
  let fn acc (pattern, behavior) =
    let pattern = pattern_to_module pattern in
    let fn = Norm.split ~pattern ~behavior in
    Seq.flat_map fn acc in
  Seq.fold_left fn (List.to_seq [ str ]) (List.to_seq lst)
