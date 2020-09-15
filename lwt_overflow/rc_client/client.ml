open Core
module OPC = Ocamlpaxos.Client
module EB = EndianBytes.LittleEndian
module MT = Message_types
module MP = Message_pb
module PD = Pbrt.Decoder
module PE = Pbrt.Encoder

let src = Logs.Src.create "client" ~doc:"logger"

module L = (val Logs.src_log src : Logs.LOG)

module Potpori = struct
  let lwt_array_all vps =
    match Array.length vps with
    | 0 ->
        raise (Invalid_argument "Array must be non-empty")
    | ei ->
        let%lwt v = vps.(0) in
        (* Grab first element in order to avoid double allocations *)
        let rs = Array.create ~len:(Array.length vps) v in
        let rec loop i =
          if i >= ei then Lwt.return rs
          else
            let p = vps.(i) in
            match Lwt.state p with
            | Lwt.Sleep ->
                let%lwt v = p in
                rs.(i) <- v ;
                (loop [@tailcall]) (i + 1)
            | Lwt.Return v ->
                rs.(i) <- v ;
                (loop [@tailcall]) (i + 1)
            | Lwt.Fail e ->
                Lwt.fail e
        in
        loop 1

  let lwt_array_map_s vps ~f =
    let%lwt v = f vps.(0) in
    let rs = Array.create ~len:(Array.length vps) v in
    let ei = Array.length vps in
    let rec loop = function
      | i when i >= ei ->
          Lwt.return rs
      | i ->
          let%lwt v = f vps.(i) in
          rs.(i) <- v ;
          loop (i + 1)
    in
    loop 1

  let lwt_array_iteri_s vps ~f =
    let rec loop = function
      | i when i >= Array.length vps ->
          Lwt.return_unit
      | i ->
          Lwt.bind (f i vps.(i)) (fun () -> (loop [@tailcall]) (i + 1))
    in
    loop 0
end

module Client = struct
  type t = OPC.t

  let put client key value cid start_time =
    let st = Unix.gettimeofday () in
    let%lwt err =
      match%lwt OPC.op_write client key value with
      | Ok _res ->
          Lwt.return ""
      | Error (`Msg e) ->
          L.err (fun m -> m "Operation failed with %s" e) ;
          Lwt.return e
    in
    let end_ = Unix.gettimeofday () in
    Lwt.return
      MT.
        { response_time= end_ -. st
        ; client_start= st
        ; queue_start= start_time
        ; end_
        ; clientid= cid
        ; optype= ""
        ; target= ""
        ; err }

  let get client key cid start_time =
    let st = Unix.gettimeofday () in
    let%lwt err =
      match%lwt OPC.op_read client key with
      | Ok _res ->
          Lwt.return ""
      | Error (`Msg e) ->
          L.err (fun m -> m "Operation failed with %s" e) ;
          Lwt.return e
    in
    let end_ = Unix.gettimeofday () in
    Lwt.return
      MT.
        { response_time= end_ -. st
        ; client_start= st
        ; queue_start= start_time
        ; end_
        ; clientid= cid
        ; optype= ""
        ; target= ""
        ; err }
end

type client = Client.t

let perform op client cid start_time i =
  L.debug (fun m -> m "Submitting request %d" i) ;
  let%lwt res =
    let keybuf = Bytes.create 8 in
    match op with
    | MT.Put {key; value} ->
        Stdlib.Bytes.set_int64_ne keybuf 0 key ;
        Client.put client keybuf value cid start_time
    | MT.Get {key} ->
        Stdlib.Bytes.set_int64_ne keybuf 0 key ;
        Client.get client keybuf cid start_time
  in
  L.info (fun m -> m "Performed op %d" i) ;
  Lwt.return res

let send payload outfile =
  let p_len = Bytes.length payload in
  let payload_buf = Bytes.create (p_len + 4) in
  Bytes.blit ~src:payload ~src_pos:0 ~dst:payload_buf ~dst_pos:4 ~len:p_len ;
  let p_len = Int32.of_int_exn p_len in
  EB.set_int32 payload_buf 0 p_len ;
  Lwt_io.write_from_exactly outfile payload_buf 0 (Bytes.length payload_buf)

let run client id op_list =
  let op_array = Array.of_list op_list in
  Array.sort op_array ~compare:(fun a b -> Float.compare a.MT.start b.MT.start) ;
  let op_list = Array.to_list op_array in
  L.info (fun m -> m "EXECUTE - Start") ;
  match `Lwt_unix_scheduling with
  | `Stream_iter_n ->
      let q = Queue.create () in
      let r_i = ref 0 in
      let start_time = Unix.gettimeofday () in
      let iter MT.{start; op_type= op; _} =
        let delay = start_time +. start -. Unix.gettimeofday () in
        let%lwt () =
          if Float.(delay <= 0.) then Lwt.return_unit else Lwt_unix.sleep delay
        in
        let%lwt res =
          perform op client id (Unix.gettimeofday ()) (Int.incr r_i ; !r_i)
        in
        Queue.enqueue q res ; Lwt.return_unit
      in
      let op_stream = Lwt_stream.of_list op_list in
      let%lwt () = Lwt_stream.iter_n ~max_concurrency:1000 iter op_stream in
      let _res = Queue.to_list q |> Array.of_list in
      L.info (fun m -> m "Finished applying ops") ;
      Lwt.return_unit
  | `Lwt_unix_scheduling ->
      let rs = Array.create ~len:(List.length op_list) (Lwt.task () |> fst) in
      let start_time = Unix.gettimeofday () in
      let rec iter n = function
        | [] ->
            Lwt.return_unit
        | MT.{start; op_type= op; _} :: remainder ->
            rs.(n) <-
              (let delay = start_time +. start -. Unix.gettimeofday () in
               let%lwt () =
                 if Float.(delay <= 0.) then Lwt.return_unit
                 else Lwt_unix.sleep delay
               in
               perform op client id (Unix.gettimeofday ()) n) ;
            (iter [@tailcall]) (n + 1) remainder
      in
      let%lwt () = iter 0 op_list in
      let%lwt _res = Potpori.lwt_array_all rs in
      L.info (fun m -> m "Finished applying ops") ;
      Lwt.return_unit
  | `Iter_potpori ->
      let vps =
        Array.create ~len:(Array.length op_array) (Lwt.task () |> fst)
      in
      let start_time = Unix.gettimeofday () in
      let dispatch i MT.{start; op_type= op; _} =
        let sleep_time = start +. start_time -. Unix.gettimeofday () in
        let%lwt () = Lwt_unix.sleep sleep_time in
        let p = perform op client id (Unix.gettimeofday ()) i in
        vps.(i) <- p ; Lwt.return_unit
      in
      let%lwt () = Potpori.lwt_array_iteri_s op_array ~f:dispatch in
      let%lwt _res = Potpori.lwt_array_all vps in
      L.info (fun m -> m "Finished applying ops") ;
      Lwt.return_unit
  | `Stream_iter_n_no_sleep ->
      let q = Queue.create () in
      let r_i = ref 0 in
      let iter MT.{op_type= op; _} =
        let%lwt res =
          perform op client id (Unix.gettimeofday ()) (Int.incr r_i ; !r_i)
        in
        Queue.enqueue q res ; Lwt.return_unit
      in
      let op_stream = Lwt_stream.of_list op_list in
      let%lwt () = Lwt_stream.iter_n ~max_concurrency:1000 iter op_stream in
      let _res = Queue.to_list q |> Array.of_list in
      L.info (fun m -> m "Finished applying ops") ;
      Lwt.return_unit

let main addrs cid ops_list =
  let%lwt client = OPC.new_client ~cid:(Int64.of_int32 cid) addrs () in
  run client cid ops_list

let reporter =
  let report src level ~over k msgf =
    let k _ = over () ; k () in
    let src = Logs.Src.name src in
    msgf
    @@ fun ?header ?tags:_ fmt ->
    Fmt.kpf k Fmt.stdout
      ("[%a] %a %a @[" ^^ fmt ^^ "@]@.")
      Time.pp (Time.now ())
      Fmt.(styled `Magenta string)
      (Printf.sprintf "%14s" src)
      Logs_fmt.pp_header (level, header)
  in
  {Logs.report}

let () =
  Lwt_engine.set (new Lwt_engine.libev ()) ;
  Fmt_tty.setup_std_outputs () ;
  Logs.(set_level (Some Info)) ;
  Logs.set_reporter reporter ;
  Printexc.record_backtrace true ;
  let addrs =
    [
      match Unix_capnp_messaging.Conn_manager.addr_of_string "127.0.0.1:5001" with
      | Ok addr ->
        (Int64.of_int 1, addr)
      | Error (`Msg e) ->
        Fmt.failwith "Expected id:(ip:port|path) not %s" e
    ]
  in
  let cid = 15 |> Int.to_int32_trunc in
  let throughput = 300000. in
  let duration = 5. in
  let n_ops = Float.(throughput * duration |> to_int) in
  L.info (fun m -> m "Doing %d ops" n_ops);
  let key = Int64.one in
  let value = "asdf" |> Bytes.of_string in
  let op = MT.{key; value} in
  let ops_list = List.init n_ops ~f:(fun i -> 
      let prereq = false in
      let start =
        let open Float in
        let i = of_int i in
        i / throughput
      in
      let op_type = MT.Put op in
      MT.{prereq; start; op_type}
    )
  in
  Lwt_main.run (
    let%lwt client = OPC.new_client ~cid:(Int64.of_int32 cid) addrs () in
    run client cid ops_list
  )
