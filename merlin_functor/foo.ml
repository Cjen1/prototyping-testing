module type S = sig
  type t

  val foo : int -> t
end

module Functor (S: S) : sig
  val bar : int -> S.t 
end = struct
  let bar i = 
     S.foo i
  end
