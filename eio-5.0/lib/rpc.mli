open! Eio

module Client : sig
  type t

  val connect : sw:Switch.t -> #Flow.two_way -> #Time.clock -> t
  val issue : t -> Cstruct.t -> Cstruct.t Promise.t
end

module Server : sig
  type t

  val create : sw:Switch.t -> #Flow.two_way -> #Time.clock -> t
  val recv : t -> Line_protocol.packet
  val send : t -> Line_protocol.packet -> unit
  val is_closed : t -> bool
end
