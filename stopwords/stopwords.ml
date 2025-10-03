let words =
  [
    ("arabic", Arabic.words);
    ("danish", Danish.words);
    ("dutch", Dutch.words);
    ("english", English.words);
    ("finnish", Finnish.words);
    ("french", French.words);
    ("german", German.words);
    ("hungarian", Hungarian.words);
    ("indonesian", Indonesian.words);
    ("italian", Italian.words);
    ("norwegian", Norwegian.words);
    ("portuguese", Portuguese.words);
    ("russian", Russian.words);
    ("spanish", Spanish.words);
    ("swedish", Swedish.words);
  ]

let words =
  let languages = Snowball.languages in
  let fn (language : Snowball.Language.t) =
    let fn (str, _words) = (language :> string) = str in
    match List.find_opt fn words with
    | Some (_, words) -> Some (language, words)
    | None -> None in
  List.filter_map fn languages
