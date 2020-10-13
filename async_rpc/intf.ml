open! Core
open! Async

let echo_api =
  Rpc.Rpc.create
    ~name:"echo"
    ~version:0
    ~bin_query:String.bin_t
    ~bin_response:String.bin_t

