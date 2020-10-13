open! Core 
open! Async
open! Intf

type test_res = {n_server: int; throughput: float; latencies: float array; starts: float array; ends: float array}[@@deriving yojson]

(* test_res Fmt.t*)
let pp_stats =
  let open Owl.Stats in
  let open Fmt in
  let fields =
    [ field "throughput" (fun s -> s.throughput) float
    ; field "n_server" (fun s -> s.n_server) int
    ; field "mean" (fun s -> mean s.latencies) float
    ; field "p50" (fun s -> percentile s.latencies 50.) float
    ; field "p75" (fun s -> percentile s.latencies 75.) float
    ; field "p99" (fun s -> percentile s.latencies 99.) float ]
  in
  record fields

let dispatch_reqs conns reqs start period =
  let rec loop dispatched i rem =
    match rem with
    | [] -> return dispatched
    | req :: rem -> 
      let%bind () = 
        let open Time in
        let wait_time = Float.(of_int i * period) in 
        let time_to_wait = diff (add start (Time.Span.of_sec wait_time)) (now ()) in
        if Time.Span.is_negative time_to_wait then
          Deferred.unit
        else after time_to_wait
      in 
      let p = 
        let start = Time.now() |> Time.to_span_since_epoch |> Time.Span.to_sec in
        let%bind res = 
          let reqs = List.map conns ~f:(fun conn -> Rpc.Rpc.dispatch_exn echo_api conn req) in
          Deferred.any reqs
        in 
        let ed = Time.now() |> Time.to_span_since_epoch |> Time.Span.to_sec in
        return (res, (start, ed))
      in 
      loop (p :: dispatched) (i + 1) rem
  in 
  let%map res = loop [] 0 reqs in
  List.rev res

let connect addresses ~f =
  print_endline @@ Fmt.str "Connecting to %a" Fmt.(list (Host_and_port.pp)) addresses; 
  let rec loop f args addrs = 
    match addrs with
    | [] -> f args
    | address :: addrs ->
      Tcp.with_connection
        (Tcp.Where_to_connect.of_host_and_port
           address)
        ~timeout:(sec 1.)
        (fun _ r w -> 
           match%bind Rpc.Connection.create r w ~connection_state:(fun _ -> ()) with
           | Error exn -> raise exn
           | Ok conn ->
             loop f (conn :: args) addrs)
  in loop f [] addresses

let output_result file res =
  let json = test_res_to_yojson res in
  Yojson.Safe.to_file file json

let client ~ports ~target_rate =
  print_endline "Starting client";
  let request_number = 500000 in
  let addresses = List.map ports ~f:(fun port -> Host_and_port.create ~host:"localhost" ~port) in
  connect addresses ~f:(fun conns ->
         let request_strings = List.init request_number ~f:(fun _ -> String.make 10 'c') in
         let response_strings = List.map request_strings ~f:(fun s -> "echo" ^ s) in
         let start = Time.now () in
         let%bind dispatched = (dispatch_reqs conns request_strings (Time.now()) Float.(1./target_rate)) in
         let%bind res = Deferred.all dispatched in
         let res_str = List.map ~f:fst res in
         let latencies = List.map ~f:snd res |> List.map ~f:(fun (st,ed) -> ed -. st) |> Array.of_list in
         let starts, ends = List.map ~f:snd res |> Array.of_list |> Array.unzip in
         let ed = Time.now () in
         assert (List.equal (String.equal) response_strings res_str);
         let duration = Time.(abs_diff start ed |> Span.to_sec ) in
         let throughput = Float.(of_int request_number / duration)  in
         let res = {n_server= List.length ports; throughput; latencies; starts; ends} in
         print_endline @@ Fmt.str "Results: \n%a" pp_stats res;
         let file = Fmt.str "%d.json" (List.length ports) in
         print_endline "Writing result to file";
         output_result file res;
         print_endline "Written";
         Deferred.unit
    )

let () =
  Command.async_spec
    ~summary:"A trivial Async-RPC server"
    Command.Spec.(
      empty 
      +> flag "-p" ~doc:" Port to send requests to" (listed int)
      +> flag "-rate" ~doc:" Rate to run client at" (optional_with_default 80000. float)
    )
    (fun ports target_rate () -> 
         client ~ports ~target_rate
    )
  |> Command.run
;;
