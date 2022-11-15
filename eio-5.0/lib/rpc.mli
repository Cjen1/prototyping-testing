open! Eio

module Client : sig
  type t

  val connect : sw:Switch.t -> ?initial_size:int -> #Flow.two_way -> t
  val issue : t -> Cstruct.t -> Cstruct.t Promise.t
  val close : t -> unit
end

module Server : sig
  type t

  val create : sw:Switch.t -> ?initial_size:int -> #Flow.two_way -> t
  val close : t -> unit
  val recv : t -> Line_protocol.packet
  val send : t -> Line_protocol.packet -> unit
  val is_closed : t -> bool
end
