let uchar_to_string uchr =
  let buf = Buffer.create 0x10 in
  Uutf.Buffer.add_utf_8 buf uchr ;
  Buffer.contents buf

let find_matches ?(encoding = `UTF_8) ~is seq =
  let decoder = Uutf.decoder ~encoding `Manual in
  let buf = Buffer.create 0x100 in
  let rec go seq () =
    match Uutf.decode decoder with
    | `Await -> begin
        match Seq.uncons seq with
        | Some (str, seq) ->
            let buf = Bytes.unsafe_of_string str in
            let len = Bytes.length buf in
            Uutf.Manual.src decoder buf 0 len ;
            go seq ()
        | None ->
            Uutf.Manual.src decoder Bytes.empty 0 0 ;
            go Seq.empty ()
      end
    | `Malformed str ->
        let token = { S.str; is_match = false } in
        Seq.Cons (token, go seq)
    | `Uchar uchr when is uchr && Buffer.length buf > 0 ->
        let tok0 = { S.str = uchar_to_string uchr; is_match = true } in
        let tok1 = { S.str = Buffer.contents buf; is_match = false } in
        Buffer.clear buf ;
        Seq.Cons (tok1, fun () -> Seq.Cons (tok0, go seq))
    | `Uchar uchr when is uchr ->
        let tok0 = { S.str = uchar_to_string uchr; is_match = true } in
        Seq.Cons (tok0, go seq)
    | `Uchar uchr ->
        Uutf.Buffer.add_utf_8 buf uchr ;
        go seq ()
    | `End when Buffer.length buf > 0 ->
        let tok0 = { S.str = Buffer.contents buf; is_match = false } in
        Seq.Cons (tok0, fun () -> Seq.Nil)
    | `End -> Seq.Nil in
  go seq
