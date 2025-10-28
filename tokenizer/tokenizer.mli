type behavior = Norm.behavior =
  | Remove
  | Isolate
  | Merge_with_previous
  | Merge_with_next

type pattern = Whitespace | Dash | Bert | Regex of Re.re
type action = pattern * Norm.behavior

val run :
  ?encoding:Snowball.encoding ->
  ?to_lowercase:bool ->
  action list ->
  string Seq.t ->
  string Seq.t
(** [run actions str] applies [actions] on the given [str]. *)

val run_on_bstr :
  ?encoding:Snowball.encoding ->
  ?to_lowercase:bool ->
  action list ->
  Bstr.t ->
  string Seq.t
(** [run actions bstr] applies [actions] on the given [bstr].

    {b NOTE}: The advantage of using a {i bigstring} is that you can load a file
    using [Unix.map_file] and process the file to obtain a stream of tokens. *)
