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

module Connection = struct
  let max_buf = 1024

  type t = { mutable enqueued : int; r : Buf_read.t; w : Buf_write.t }

  let with_connection (flow : #Eio.Flow.two_way) f =
    Buf_write.with_flow flow (fun w ->
        let t =
          { enqueued = 0; r = Buf_read.of_flow ~max_size:1_000_000 flow; w }
        in
        f t)

  let is_closed t = Buf_read.at_end_of_input t.r || Buf_write.is_closed t.w

  let send t (p : LineProtocol.packet) =
    if t.enqueued < max_buf then ()
    else (
      t.enqueued <- 0;
      Buf_write.flush t.w);
    t.enqueued <- t.enqueued + 2;
    LineProtocol.write_packet p t.w

  let recv t = LineProtocol.read_packet t.r
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

module RpcService = struct
  open Core
  module UId = UniqueId32 ()
  module ITbl = Hashtbl.Make (UId)

  type t = { conn : Connection.t; fulfillers : Cstruct.t Promise.u ITbl.t }

  let handler t =
    let rec loop () =
      let pkt = Connection.recv t.conn in
      let f = ITbl.find t.fulfillers pkt.id in
      match f with
      | None -> raise_s [%message "no matching id" (pkt.id : int32)]
      | Some f ->
          Promise.resolve f pkt.payload;
          loop ()
    in
    loop

  let create ~sw conn =
    let t = { conn; fulfillers = ITbl.create () } in
    Fiber.fork ~sw (handler t);
    t

  let issue t payload : Cstruct.t Promise.t =
    let p, u = Promise.create () in
    let id = UId.create () in
    let pkt = LineProtocol.{ id; payload } in
    ITbl.set t.fulfillers ~key:id ~data:u;
    Connection.send t.conn pkt;
    p
end
