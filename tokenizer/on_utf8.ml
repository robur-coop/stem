let uchar_to_string uchr =
  let buf = Buffer.create 0x10 in
  Uutf.Buffer.add_utf_8 buf uchr ;
  Buffer.contents buf

let uchars_to_string uchrs =
  let buf = Buffer.create 0x10 in
  List.iter (Uutf.Buffer.add_utf_8 buf) (List.rev uchrs) ;
  Buffer.contents buf

let find_matches ~is ?(off = 0) ?len str =
  let fn acc pos elt =
    match (acc, elt) with
    | (None, acc), `Malformed _ ->
        let token = { S.str = String.make 1 str.[pos]; is_match = false } in
        (None, token :: acc)
    | (Some seg, acc), `Malformed _ ->
        let token0 = { S.str = uchars_to_string seg; is_match = false } in
        let token1 = { S.str = String.make 1 str.[pos]; is_match = false } in
        (None, token1 :: token0 :: acc)
    | (None, acc), `Uchar uchr when is uchr ->
        let token = { S.str = uchar_to_string uchr; is_match = true } in
        (None, token :: acc)
    | (None, acc), `Uchar uchr -> (Some [ uchr ], acc)
    | (Some seg, acc), `Uchar uchr when is uchr ->
        let token0 = { S.str = uchars_to_string seg; is_match = false } in
        let token1 = { S.str = uchar_to_string uchr; is_match = true } in
        (None, token1 :: token0 :: acc)
    | (Some seg, acc), `Uchar uchr -> (Some (uchr :: seg), acc) in
  let lst =
    Uutf.String.fold_utf_8 ~pos:off ?len fn (None, []) str |> function
    | None, acc -> List.rev acc
    | Some seg, acc ->
        let token = { S.str = uchars_to_string seg; is_match = false } in
        List.rev (token :: acc) in
  List.rev lst |> List.to_seq
