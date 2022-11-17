open! Eio

type packet_header = Cstruct.t
type packet = { id : int32; payload : Cstruct.t }

val read_packet : packet Buf_read.parser
val write_packet : packet -> Buf_write.t -> unit

val to_cstructs : packet -> Cstruct.t list
