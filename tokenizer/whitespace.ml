let find_matches ?(encoding = Snowball.UTF_8) =
  match encoding with
  | Snowball.UTF_8 -> On_utf8.find_matches ~is:Uucp.White.is_white_space
  | encoding ->
      Fmt.invalid_arg "Unimplemented encoding: %a" Snowball.pp_encoding encoding
