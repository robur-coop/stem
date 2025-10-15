type 'a t = {
  buffer : 'a option array;
  mutable rd_pos : int;
  mutable wr_pos : int;
  lock : Miou.Mutex.t;
  non_empty : Miou.Condition.t;
  non_full : Miou.Condition.t;
  mutable closed : bool;
}

let create size =
  let lock = Miou.Mutex.create () in
  let non_empty = Miou.Condition.create () in
  let non_full = Miou.Condition.create () in
  {
    buffer = Array.make size None;
    lock;
    rd_pos = 0;
    wr_pos = 0;
    non_empty;
    non_full;
    closed = false;
  }

let put t data =
  Miou.Mutex.protect t.lock @@ fun () ->
  if t.closed then invalid_arg "Stream.put closed stream" ;
  while (t.wr_pos + 1) mod Array.length t.buffer = t.rd_pos do
    Miou.Condition.wait t.non_full t.lock
  done ;
  t.buffer.(t.wr_pos) <- Some data ;
  t.wr_pos <- (t.wr_pos + 1) mod Array.length t.buffer ;
  Miou.Condition.signal t.non_empty

let get t =
  Miou.Mutex.protect t.lock @@ fun () ->
  while t.wr_pos = t.rd_pos && not t.closed do
    Miou.Condition.wait t.non_empty t.lock
  done ;
  if t.closed && t.wr_pos = t.rd_pos
  then None
  else
    let data = t.buffer.(t.rd_pos) in
    t.buffer.(t.rd_pos) <- None ;
    t.rd_pos <- (t.rd_pos + 1) mod Array.length t.buffer ;
    Miou.Condition.signal t.non_full ;
    data

let close t =
  Miou.Mutex.protect t.lock @@ fun () ->
  t.closed <- true ;
  Miou.Condition.signal t.non_empty

let of_seq ?(parallel = false) size seq =
  let t = create size in
  let fn () =
    Seq.iter (put t) seq ;
    close t in
  if parallel then (t, Miou.call fn) else (t, Miou.async fn)
