let is_ascii_punctuation = function
  | '!' | '\"' | '#' | '$' | '%' | '&' | '\'' | '(' | ')' | '*' | '+' | ','
  | '-' | '.' | '/' | ':' | ';' | '<' | '=' | '>' | '?' | '@' | '[' | '\\' | ']'
  | '^' | '_' | '`' | '{' | '|' | '}' | '~' ->
      true
  | _ -> false

let is_punctuation uchr =
  if Uchar.is_char uchr
  then is_ascii_punctuation (Uchar.to_char uchr)
  else
    match Uucp.Gc.general_category uchr with
    | `Pc | `Pd | `Ps | `Pe | `Pi | `Pf | `Po -> true
    | _ -> false

let find_matches ?(encoding = Snowball.UTF_8) =
  match encoding with
  | Snowball.UTF_8 -> On_utf8.find_matches ~is:is_punctuation
  | encoding ->
      Fmt.invalid_arg "Unimplemented encoding: %a" Snowball.pp_encoding encoding
