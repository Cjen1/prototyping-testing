open! Eio

module UniqueId32 () = struct
  open Core
  include Int32

  let rec race_free_create_loop cell make =
    let x = !cell in
    let new_x = make x in
    if !cell = x then (
      cell := new_x;
      x)
    else race_free_create_loop cell make

  let current = ref zero
  let create () = race_free_create_loop current succ
end

module UId = UniqueId32 ()
module ITbl = Core.Hashtbl.Make (UId)

module Writer = struct
  type t = {
    buf : Cstruct.t Queue.t;
    max_size : int;
    flow : Flow.sink;
    clock : Eio.Time.clock;
    mutable last_flush : float;
  }

  let take_n t target =
    let rec loop n acc =
      if n = 0 then (acc, 0)
      else if Queue.is_empty t.buf then (acc, target - n)
      else loop (n - 1) (Queue.pop t.buf :: acc)
    in
    loop target []

  let flush t =
    t.last_flush <- Time.now t.clock;
    let iovecs, len = take_n t t.max_size in
    if len > 0 then (
      Flow.write t.flow iovecs;
    )

  let space_available t n = Queue.length t.buf + n < t.max_size

  let write t cs =
    if not @@ space_available t (List.length cs) then (
      flush t);
    let rec loop = function
      | [] -> ()
      | c :: cs ->
          Queue.add c t.buf;
          loop cs
    in
    loop cs

  let create ~sw ?(max_size = 1024) ?(max_time_to_flush = 0.01) flow clock =
    let t =
      {
        buf = Queue.create ();
        max_size;
        flow;
        clock;
        last_flush = Time.now clock;
      }
    in
    traceln "Created new writer : {last_flush=%f}" t.last_flush;
    Fiber.fork_daemon ~sw (fun () ->
        while true do
          let sleep_target = Float.add t.last_flush max_time_to_flush in
          let sleep_duration = Float.sub sleep_target (Time.now clock) in
          if sleep_duration > 0. then Time.sleep clock sleep_duration;
          let current = Time.now clock in
          let duration_since_last_flush = Float.sub current t.last_flush in
          if duration_since_last_flush >= max_time_to_flush then flush t
        done;
        assert false);
    Switch.on_release sw (fun () -> flush t);
    t
end

type t = {
  r : Buf_read.t;
  w : Writer.t;
  fulfillers : Cstruct.t Promise.u ITbl.t;
}

let send t (pkt : Line_protocol.packet) =
  Writer.write t.w (Line_protocol.to_cstructs pkt)

let recv t = 
  let r = Line_protocol.read_packet t.r in
  r


let handler t () =
  let rec loop () =
    let open Core in
    let pkt = recv t in
    let f = ITbl.find_and_remove t.fulfillers pkt.id in
    match f with
    | None -> raise_s [%message "no matching id" (pkt.id : int32)]
    | Some f ->
        Promise.resolve f pkt.payload;
        loop ()
  in
  loop ()

let create ~kind ~sw (flow : #Eio.Flow.two_way) (clock : #Eio.Time.clock) =
  let t_p, t_u = Promise.create () in
  Fiber.fork ~sw (fun () ->
      let w = Writer.create ~sw (flow :> Flow.sink) (clock :> Time.clock) in
      let r = Buf_read.of_flow ~max_size:1_000_000 flow in
      let t = { r; w; fulfillers = ITbl.create () } in
      match kind with
      | `Server -> Promise.resolve t_u t
      | `Client ->
          Fiber.fork_daemon ~sw (handler t);
          Promise.resolve t_u t);
  let t = Promise.await t_p in
  t

module Client = struct
  type nonrec t = t

  let connect ~sw (flow : #Eio.Flow.two_way) (clock : #Eio.Time.clock) =
    create ~kind:`Client ~sw flow clock

  let issue t payload : Cstruct.t Promise.t =
    let p, u = Promise.create () in
    let id = UId.create () in
    let pkt = Line_protocol.{ id; payload } in
    ITbl.set t.fulfillers ~key:id ~data:u;
    send t pkt;
    p
end

module Server = struct
  type nonrec t = t

  let create ~sw (flow : #Eio.Flow.two_way) (clock : #Eio.Time.clock) =
    create ~kind:`Server ~sw flow clock

  let recv = recv
  let send = send
  let is_closed t = Buf_read.at_end_of_input t.r
end
