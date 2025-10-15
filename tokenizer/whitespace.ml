let find_matches ?(encoding = Snowball.UTF_8) str =
  let seq = Seq.return str in
  let is = Uucp.White.is_white_space in
  match encoding with
  | Snowball.UTF_8 -> On_utf8.find_matches ~encoding:`UTF_8 ~is seq
  | Snowball.ISO_8859_1 -> On_utf8.find_matches ~encoding:`ISO_8859_1 ~is seq
  | encoding ->
      Fmt.invalid_arg "Unimplemented encoding: %a" Snowball.pp_encoding encoding

let find_matches_on_bstr ?(encoding = Snowball.UTF_8) bstr =
  let is = Uucp.White.is_white_space in
  match encoding with
  | Snowball.UTF_8 -> On_utf8.find_matches_on_bstr ~encoding:`UTF_8 ~is bstr
  | Snowball.ISO_8859_1 ->
      On_utf8.find_matches_on_bstr ~encoding:`ISO_8859_1 ~is bstr
  | encoding ->
      Fmt.invalid_arg "Unimplemented encoding: %a" Snowball.pp_encoding encoding
