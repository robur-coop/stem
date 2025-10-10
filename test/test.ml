let tok00 =
  Alcotest.test_case "dash tokenizer" `Quick @@ fun () ->
  let str = Seq.return "the-final--countdown" in
  let _1 = Tokenizer.run [ (Dash, Merge_with_previous) ] str |> List.of_seq in
  Alcotest.(check (list string))
    "merge-with-previous" _1
    [ "the-"; "final-"; "-"; "countdown" ] ;
  let _2 = Tokenizer.run [ (Dash, Merge_with_next) ] str |> List.of_seq in
  Alcotest.(check (list string))
    "merge-with-next" _2
    [ "the"; "-final"; "-"; "-countdown" ] ;
  let _3 = Tokenizer.run [ (Dash, Remove) ] str |> List.of_seq in
  Alcotest.(check (list string)) "remove" _3 [ "the"; "final"; "countdown" ] ;
  let _4 = Tokenizer.run [ (Dash, Isolate) ] str |> List.of_seq in
  Alcotest.(check (list string))
    "isolate" _4
    [ "the"; "-"; "final"; "-"; "-"; "countdown" ]

let tok01 =
  Alcotest.test_case "bert tokenizer" `Quick @@ fun () ->
  let str = Seq.return "Hey friend!     How are you?!?" in
  let actions = Tokenizer.[ (Bert, Isolate); (Whitespace, Remove) ] in
  let lst = Tokenizer.run actions str |> List.of_seq in
  Alcotest.(check (list string))
    "bert" lst
    [ "Hey"; "friend"; "!"; "How"; "are"; "you"; "?"; "!"; "?" ]

let () = Alcotest.run "stem" [ ("tokenizer", [ tok00; tok01 ]) ]
