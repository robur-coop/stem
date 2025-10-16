type config
type 'uid t
type 'uid file = 'uid * [ `Contents of Bstr.t | `File of string ]

val config :
  ?parallel:bool ->
  ?encoding:Snowball.encoding ->
  ?actions:Tokenizer.action list ->
  Snowball.Language.t ->
  config

val make : cfg:config -> ?k1:float -> ?b:float -> 'uid file list -> 'uid t
val rank : 'uid t -> string -> ('uid * float) list
