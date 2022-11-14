open! Eio

module LineProtocol : sig
  type packet = { id : int32; payload : Cstruct.t }
end

module Connection : sig
  type t

  val with_connection : #Flow.two_way -> (t -> 'a) -> 'a
  val is_closed : t -> bool
  val send : t -> LineProtocol.packet -> unit
  val recv : t -> LineProtocol.packet
end

module RpcService : sig
  type t

  val create : sw:Switch.t -> Connection.t -> t
  val issue : t -> Cstruct.t -> Cstruct.t Promise.t
end
