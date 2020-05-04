
module CalcInterface(R : Idl.RPC) = struct
  open R

  let int_p = Idl.Param.mk Rpc.Types.int

  let add = R.declare "add"
      ["Add two numbers"]
      (int_p @-> int_p @-> returning int_p Idl.DefaultError.err)

  let mul = R.declare "mul"
      ["Multiply two numbers"]
      (int_p @-> int_p @-> returning int_p Idl.DefaultError.err)

  let implementation = implement
      { Idl.Interface.name = "Calc"; namespace = Some "Calc"; description = ["Calculator supporting addition and multiplication"]; version = (1,0,0) }
end

let rpc = assert false

module CalcClient :
  sig
    val add : int -> int -> int
    val mul : int -> int -> int
  end = CalcInterface(Idl.GenClientExnRpc (struct let rpc = rpc end))
