type config
type 'uid t

type 'uid file =
  'uid * [ `Contents of Bstr.t | `File of string | `String of string ]
(** Type of documents with their contents. *)

val config :
  ?parallel:bool ->
  ?encoding:Snowball.encoding ->
  ?actions:Tokenizer.action list ->
  Snowball.Language.t ->
  config
(** [config ?parallel ?encoding ?actions language] creates a configuration that
    allows the search engine to determine several parameters:
    - [parallel] allows the calculation of occurrences to be decoupled from the
      {i tokenisation} of a document
    - [encoding] specifies the encoding used for documents ([UTF_8] by default).
    - [actions] specifies the actions for tokenisation (for documents as well as
      the search query). By default, this consists of removing spaces and
      punctuation marks.
    - [language] specifies the language of the documents (and how to obtain the
      root of words). *)

val make : cfg:config -> ?k1:float -> ?b:float -> 'uid file list -> 'uid t
(** [make ~cfg ?k1 ?b documents] collects all occurrences in the given
    [documents]. This creates a {i search space} that can be used to search for
    certain words and obtain a score (with {!val:score}) for each document
    (higher or lower depending on the relevance of the document to the words
    being searched for). *)

val score : 'uid t -> string -> ('uid * float) list
(** [score t query] calculates the relevance of each document in relation to a
    given search query. The query must be in the same language (and encoding) as
    the given documents. *)
