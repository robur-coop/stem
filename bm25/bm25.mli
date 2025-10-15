type config
type t

val config :
  ?parallel:bool ->
  ?encoding:Snowball.encoding ->
  ?actions:Tokenizer.action list ->
  Snowball.Language.t ->
  config

val make : cfg:config -> ?k1:float -> ?b:float -> string list -> t
val rank : t -> string -> (string * float) list
