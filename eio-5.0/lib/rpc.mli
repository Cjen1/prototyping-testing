open! Eio

module LineProtocol : sig
  type packet = { id : int32; payload : Cstruct.t }
end

module Rpc : sig
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

    val recv : t -> LineProtocol.packet
    val send : t -> LineProtocol.packet -> unit
    val is_closed : t -> bool
  end
end
