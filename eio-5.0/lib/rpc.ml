open! Eio

module LineProtocol = struct
  open! Buf_read
  open! Buf_read.Syntax

  type packet_header = Cstruct.t

  [%%cstruct
  type packet_header = { id : uint32_t; length : uint16_t } [@@big_endian]]

  let header : packet_header parser =
    let+ b = take sizeof_packet_header in
    Cstruct.of_string b

  type packet = { id : int32; payload : Cstruct.t }

  let read_packet : packet parser =
    let* header = header in
    let* payload =
      let+ s = take (get_packet_header_length header) in
      Cstruct.of_string s
    in
    return { payload; id = get_packet_header_id header }

  let write_packet p w =
    let open Buf_write in
    let header = Cstruct.create_unsafe sizeof_packet_header in
    (* TODO test using built in serialisers *)
    set_packet_header_id header p.id;
    set_packet_header_length header p.payload.len;
    schedule_cstruct w header;
    schedule_cstruct w p.payload
end

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

module Rpc = struct
  open Core
  module UId = UniqueId32 ()
  module ITbl = Hashtbl.Make (UId)

  module ManBufWrite = struct
    let read_source_buffer t fn =
      let iovecs = Buf_write.await_batch t in
      Buf_write.shift t (fn iovecs)

    let read_into t buf =
      let iovecs = Buf_write.await_batch t in
      let n, _iovecs = Cstruct.fillv ~src:iovecs ~dst:buf in
      Buf_write.shift t n;
      n

    let as_flow t =
      object
        inherit Flow.source

        method! read_methods =
          [ Flow.Read_source_buffer (read_source_buffer t) ]

        method read_into = read_into t
      end

    let of_flow ~sw ?(initial_size = 0x1000) flow =
      let w = Buf_write.create ~sw initial_size in
      Fiber.fork ~sw (fun () -> Flow.copy (as_flow w) flow);
      w
  end

  let max_buf = 1024

  type t = {
    r : Buf_read.t;
    w : Buf_write.t;
    mutable enqueued : int;
    mutable state : [ `Closed | `Open ];
    fulfillers : Cstruct.t Promise.u ITbl.t;
  }

  let send t (pkt : LineProtocol.packet) =
    if t.enqueued >= max_buf then (
      t.enqueued <- 0;
      Buf_write.flush t.w);
    t.enqueued <- t.enqueued + 2;
    LineProtocol.write_packet pkt t.w

  let close t =
    match t.state with
    | `Closed -> ()
    | `Open ->
        t.state <- `Closed;
        Buf_write.close t.w

  let recv t = LineProtocol.read_packet t.r

  let handler t () =
    let rec loop () =
      let pkt = recv t in
      let f = ITbl.find_and_remove t.fulfillers pkt.id in
      match f with
      | None -> raise_s [%message "no matching id" (pkt.id : int32)]
      | Some f ->
          Promise.resolve f pkt.payload;
          loop ()
    in
    loop ()

  let create ~kind ~sw ?(initial_size = 0x1000) (flow : #Eio.Flow.two_way) =
    let t_p, t_u = Promise.create () in
    Fiber.fork_sub ~sw
      ~on_error:(fun e ->
        close (Promise.await t_p);
        raise e)
      (fun sw ->
        let w = ManBufWrite.of_flow ~initial_size ~sw flow in
        let r = Buf_read.of_flow ~max_size:1_000_000 flow in
        let t =
          { r; w; enqueued = 0; fulfillers = ITbl.create (); state = `Open }
        in
        match kind with
        | `Server -> Promise.resolve t_u t
        | `Client ->
            Fiber.fork_daemon ~sw (handler t);
            Promise.resolve t_u t);
    let t = Promise.await t_p in
    t

  module Client = struct
    type nonrec t = t

    let connect ~sw ?(initial_size = 0x1000) (flow : #Eio.Flow.two_way) =
      create ~kind:`Client ~sw ~initial_size flow

    let issue t payload : Cstruct.t Promise.t =
      let p, u = Promise.create () in
      let id = UId.create () in
      let pkt = LineProtocol.{ id; payload } in
      ITbl.set t.fulfillers ~key:id ~data:u;
      send t pkt;
      p

    let close = close
  end

  module Server = struct
    type nonrec t = t

    let create ~sw ?(initial_size = 0x1000) (flow : #Eio.Flow.two_way) =
      create ~kind:`Server ~sw ~initial_size flow

    let recv = recv
    let send = send
    let close = close

    let is_closed t =
      Buf_read.at_end_of_input t.r
      || match t.state with `Closed -> true | _ -> false
  end
end
