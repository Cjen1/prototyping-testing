open! Eio
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
  set_packet_header_id header p.id;
  set_packet_header_length header p.payload.len;
  schedule_cstruct w header;
  schedule_cstruct w p.payload

let to_cstructs p =
  let header = Cstruct.create_unsafe sizeof_packet_header in
  set_packet_header_id header p.id;
  set_packet_header_length header p.payload.len;
  [p.payload;header]

