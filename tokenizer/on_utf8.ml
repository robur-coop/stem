let uchar_to_string uchr =
  let buf = Buffer.create 0x10 in
  Uutf.Buffer.add_utf_8 buf uchr ;
  Buffer.contents buf

let find_matches ?(encoding = `UTF_8) ~is ?(off = 0) ?len str =
  let len = match len with Some len -> len | None -> String.length str - off in
  let decoder = Uutf.decoder ~encoding `Manual in
  let buf = Buffer.create 0x100 in
  Uutf.Manual.src decoder (Bytes.unsafe_of_string str) off len ;
  let rec go () =
    match Uutf.decode decoder with
    | `Await ->
        Uutf.Manual.src decoder Bytes.empty 0 0 ;
        go ()
    | `Malformed str ->
        let token = { S.str; is_match = false } in
        Seq.Cons (token, go)
    | `Uchar uchr when is uchr ->
        let go =
          if Buffer.length buf > 0
          then begin
            let token0 = { S.str = Buffer.contents buf; is_match = false } in
            Buffer.clear buf ;
            Fun.const (Seq.Cons (token0, go))
          end
          else go in
        let token1 = { S.str = uchar_to_string uchr; is_match = true } in
        Seq.Cons (token1, go)
    | `Uchar uchr ->
        Uutf.Buffer.add_utf_8 buf uchr ;
        go ()
    | `End -> Seq.Nil in
  go
