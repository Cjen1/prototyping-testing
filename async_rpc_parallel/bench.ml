open! Core 
open! Async
open! Rpc_parallel

module Stage = struct
  module T = struct

    type ping = [`Ping of string] [@@deriving bin_io]
    type pong = [`Pong of string] [@@deriving bin_io]

    type 'worker functions = {
      setup_input_ev_pipe: ('worker, unit * ping Pipe.Reader.t, unit) Function.t
    ; setup_output_ev_pipe:('worker, unit , pong Pipe.Reader.t) Function.t
    }

    module Worker_state = struct
      type init_arg = unit  [@@deriving bin_io]
      type t = {mutable reader: ping Pipe.Reader.t Ivar.t; writer : pong Pipe.Writer.t Ivar.t}
    end 

    module Connection_state = struct
      type init_arg = unit [@@deriving bin_io]
      type t = unit
    end

    module Functions (C : Rpc_parallel.Creator with type worker_state := Worker_state.t and type connection_state := Connection_state.t) = struct
      let setup_input_ev_pipe =
        C.create_reverse_pipe
          ~f:(fun ~worker_state ~conn_state:() () (ev_pipe_reader : ping Pipe.Reader.t) ->
              let%bind writer = Ivar.read worker_state.writer in
              Ivar.fill worker_state.reader ev_pipe_reader;
              Pipe.iter ev_pipe_reader  ~f:(fun (`Ping s) ->
                  Pipe.write_without_pushback writer (`Pong s : pong);
                  Deferred.unit
                ) |> don't_wait_for;
              Deferred.unit
            )
          ~bin_query:Unit.bin_t
          ~bin_update:bin_ping
          ~bin_response:Unit.bin_t
          ()

      let setup_output_ev_pipe =
        C.create_pipe
          ~f:(fun ~worker_state ~conn_state:() () ->
              let reader, writer = Pipe.create () in
              Ivar.fill worker_state.writer writer;
              return reader
            )
          ~bin_input:Unit.bin_t
          ~bin_output:bin_pong
          ()

        let functions = {setup_input_ev_pipe; setup_output_ev_pipe}

        let init_worker_state () = return Worker_state.{reader=Ivar.create (); writer = Ivar.create ()}
      let init_connection_state ~connection:_ ~worker_state:_ () = return ()
    end 
  end 

  module M =  Rpc_parallel.Make(T)
  include M

  module Shutdown_on = M.Shutdown_on(Monad.Ident)
end

let spawn_exn () =
  Stage.spawn_exn ~shutdown_on:Stage.Shutdown_on.Connection_closed ~redirect_stdout:(`Dev_null) ~redirect_stderr:(`Dev_null) () ~on_failure:(Error.raise)

type worker_pipes = {input: Stage.T.ping Pipe.Writer.t; output: Stage.T.pong Pipe.Reader.t; conn: Stage.Connection.t}

let get_streams () =
  let%bind conn = spawn_exn () ~connection_state_init_arg:() in
  let%bind output = Stage.Connection.run_exn conn ~f:Stage.functions.setup_output_ev_pipe ~arg:() in
  let rd, input = Pipe.create () in
  let%bind () = Stage.Connection.run_exn conn ~f:Stage.functions.setup_input_ev_pipe ~arg:((), rd) in
  {input; output; conn} |> return

type test_res = {target_throughput: float; throughput: float; latencies: float array; starts: float array; ends: float array}[@@deriving yojson]

(* test_res Fmt.t*)
let pp_stats =
  let open Owl.Stats in
  let open Fmt in
  let fields =
    [ field "throughput" (fun s -> s.throughput) float
    ; field "target_throughput" (fun s -> s.target_throughput) float
    ; field "mean" (fun s -> mean s.latencies) float
    ; field "p50" (fun s -> percentile s.latencies 50.) float
    ; field "p75" (fun s -> percentile s.latencies 75.) float
    ; field "p99" (fun s -> percentile s.latencies 99.) float ]
  in
  record fields

let dispatch_reqs pipes reqs start period =
  let rec loop dispatched i rem =
    match rem with
    | [] -> return dispatched
    | req :: rem -> 
      if i % 100 = 0 then print_char '.';
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
          let () = Pipe.write_without_pushback pipes.input (`Ping req) in 
          match%bind Pipe.read pipes.output with
          | `Eof -> return "EOF"
          | `Ok (`Pong v) -> return v
        in 
        let ed = Time.now() |> Time.to_span_since_epoch |> Time.Span.to_sec in
        return (res, (start, ed))
      in 
      loop (p :: dispatched) (i + 1) rem
  in 
  let%map res = loop [] 0 reqs in
  List.rev res

let output_result file res =
  let json = test_res_to_yojson res in
  Yojson.Safe.to_file file json

let rec compare_list ~equal = function
  | [], [] -> true
  | x::xs, y::ys when equal x y -> 
    compare_list ~equal (xs,ys)
  | xs, ys ->
    print_endline @@ Fmt.str "Not equal lists: [%a ...] != [%a ...]" Fmt.(list ~sep:comma string) (List.take xs 3) Fmt.(list ~sep:comma string) (List.take ys 3);
    false

let client ~target_rates =
  print_endline @@ Fmt.str "Targeting rates %a" Fmt.(list ~sep:comma (float_dsig 0)) target_rates;
  Deferred.List.iter ~how:`Sequential target_rates ~f:(fun target_rate ->
      print_endline @@ Fmt.str "Starting test for rate: %.0f" target_rate;
      let request_number = 500000 in
      let request_strings = List.init request_number ~f:(Int.to_string) in
      let%bind pipes = get_streams () in
      let start = Time.now () in
      let%bind dispatched = (dispatch_reqs pipes request_strings (Time.now()) Float.(1./target_rate)) in
      let%bind res = Deferred.all dispatched in
      let%bind () = Stage.Connection.close pipes.conn in
      let res_str = List.map ~f:fst res in
      let latencies = List.map ~f:snd res |> List.map ~f:(fun (st,ed) -> ed -. st) |> Array.of_list in
      let starts, ends = List.map ~f:snd res |> Array.of_list |> Array.unzip in
      let ed = Time.now () in
      assert (compare_list ~equal:String.equal (request_strings,res_str));
      let duration = Time.(abs_diff start ed |> Span.to_sec ) in
      let throughput = Float.(of_int request_number / duration)  in
      let res = {target_throughput= target_rate; throughput; latencies; starts; ends} in
      print_endline @@ Fmt.str "Results: \n%a" pp_stats res;
      let file = Fmt.str "%.2f.json" (target_rate) in
      print_endline "Writing result to file";
      output_result file res;
      print_endline "Written";
      Deferred.unit
    )

let () =
  Command.async_spec
    ~summary:"A trivial pipe Rpc_parallel setup"
    Command.Spec.(
      empty 
      +> flag "-rate" ~doc:" Rate to run client at" (listed float)
    )
    (fun target_rates () -> 
         client ~target_rates
    )
  |> Rpc_parallel.start_app
;;
