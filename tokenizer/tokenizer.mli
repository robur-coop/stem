type behavior = Norm.behavior =
  | Remove
  | Isolate
  | Merge_with_previous
  | Merge_with_next

type pattern = Whitespace | Dash | Bert | Regex of Re.re
type action = pattern * Norm.behavior

val run : action list -> string -> string Seq.t
(** [run actions str] applies [actions] on the given [str]. *)
