open Lwt.Infix
open Unix_capnp_messaging
open Sockets

let of_store, to_store =
  let open Capnp.BytesMessage.Message in
  let of_store msg = of_storage [ msg ] in
  let to_store msg =
    to_storage msg
    |> List.map (fun descr -> descr.segment)
    |> Bytes.concat Bytes.empty
  in
  (of_store, to_store)

let test_message =
  let buf = Bytes.create 16 in
  Bytes.set_int64_le buf 0 (Random.int64 Int64.max_int);
  Bytes.set_int64_le buf 8 (Random.int64 Int64.max_int);
  buf

let exn = Alcotest.testable Fmt.exn ( = )

let exn_handler p = p >>= function Ok v -> Lwt.return v | Error e -> raise e

exception Timeout

let timeout f = Lwt_unix.sleep f >>= fun () -> Lwt.return_error Timeout

let test_of_to_store () =
  Alcotest.(check string)
    "to-of store"
    (test_message |> Bytes.to_string)
    (test_message |> of_store |> to_store |> Bytes.to_string)

let test_loop _ () =
  let fd1, fd2 = Lwt_unix.pipe () in
  let switch = Lwt_switch.create () in
  Lwt.both (Incomming.create ~switch fd1) (Outgoing.create ~switch fd2)
  >>= fun (ins, out) ->
  test_message |> of_store |> Outgoing.send out |> exn_handler >>= fun () ->
  Lwt.choose [ timeout 1.; Incomming.recv ins ] >>= fun msg ->
  let ( >>>= ) = Result.bind in
  Alcotest.(check (result string exn))
    "Send and receive"
    (Ok (test_message |> Bytes.to_string))
    (msg >>>= fun msg -> Ok (msg |> to_store |> Bytes.to_string));
  Lwt_switch.turn_off switch

let some_wrap f () = f () >>= fun v -> Lwt.return_some v

let none_wrap f () = f () >>= fun _ -> Lwt.return_none

let test_stress size _ () =
  let fd1, fd2 = Lwt_unix.pipe () in
  let switch = Lwt_switch.create () in
  Lwt.both (Incomming.create ~switch fd1) (Outgoing.create ~switch fd2)
  >>= fun (ins, out) ->
  let test_message i =
    let buf = Bytes.create 8 in
    Bytes.set_int64_le buf 0 (Int64.of_int i);
    buf |> of_store
  in
  let max_concurrency = 1 in
  let stream = List.init size test_message |> Lwt_stream.of_list in
  let send_i msg =
    Outgoing.send out msg >>= fun res -> Result.get_ok res |> Lwt.return
  in
  let send_p = Lwt_stream.iter_n ~max_concurrency send_i stream in
  let generator () =
    Lwt.choose
      [
        some_wrap (fun () -> Incomming.recv ins) ();
        none_wrap (fun () -> timeout 5.) ();
      ]
  in
  let recv_stream =
    Lwt_stream.from generator |> Lwt_stream.map (fun v -> Result.get_ok v)
  in
  let fold v expected =
    let buf = to_store v in
    let i = Bytes.get_int64_le buf 0 in
    Alcotest.(check int)
      (Fmt.str "Checking recv %d" expected)
      (Int64.to_int i) expected;
    expected + 1
  in
  let recv_p = Lwt_stream.fold fold recv_stream 0 in
  Lwt.both send_p recv_p >>= fun ((), v) ->
  Alcotest.(check int) "Number processed is same as sent in" size v;
  Lwt_switch.turn_off switch

let test_lwt_result str test_exn res =
  match res with
  | Ok _v -> Alcotest.fail (Fmt.str "No exception raised for %s" str)
  | Error exn -> Alcotest.check_raises str test_exn (fun () -> raise exn)

let test_closed_switch _ () =
  let fd1, fd2 = Lwt_unix.pipe () in
  let switch = Lwt_switch.create () in
  Lwt.both (Incomming.create ~switch fd1) (Outgoing.create ~switch fd2)
  >>= fun (ins, out) ->
  Lwt_switch.turn_off switch >>= fun () ->
  test_message |> of_store |> Outgoing.send out >>= fun tmp ->
  test_lwt_result "Send on closed outgoing" Sockets.Closed tmp;
  Incomming.recv ins
  >|= test_lwt_result "Recv on closed incommming" Sockets.Closed

let test_closed_fd _ () =
  let fd1, fd2 = Lwt_unix.pipe () in
  let switch = Lwt_switch.create () in
  Lwt.both (Incomming.create fd1) (Outgoing.create fd2) >>= fun (ins, out) ->
  Lwt_unix.close fd1 >>= fun () ->
  test_message |> of_store |> Outgoing.send out >>= fun tmp ->
  test_lwt_result "Send on closed outgoing" Sockets.Closed tmp;
  Incomming.recv ins
  >|= test_lwt_result "Recv on closed incommming" Sockets.Closed
  >>= fun () -> Lwt_switch.turn_off switch

let reporter =
  let open Core in
  let report src level ~over k msgf =
    let k _ =
      over ();
      k ()
    in
    let src = Logs.Src.name src in
    msgf @@ fun ?header ?tags:_ fmt ->
    Fmt.kpf k Fmt.stdout
      ("[%a] %a %a @[" ^^ fmt ^^ "@]@.")
      Time.pp (Time.now ())
      Fmt.(styled `Magenta string)
      (Printf.sprintf "%14s" src)
      Logs_fmt.pp_header (level, header)
  in
  { Logs.report }

let () =
  Logs.(set_level (Some Debug));
  Logs.set_reporter reporter;
  let _ =
    Sys.signal Sys.sigpipe
      (Sys.Signal_handle
         (fun _ -> raise @@ Unix.Unix_error (Unix.EPIPE, "", "")))
  in
  let open Alcotest_lwt in
  Lwt_main.run
  @@ run "Socket test"
       [
         ("Test infra", [ test_case_sync "to/of store" `Quick test_of_to_store ]);
         ("Basic functionality", [ test_case "Loop test" `Quick test_loop ]);
         ( "Stress test",
           [
             test_case "Stressed loop" `Quick (test_stress 100);
             test_case "Stressed loop" `Slow (test_stress 1000);
           ] );
         ( "Exceptions",
           [
             test_case "Closed switch exceptions" `Quick test_closed_switch;
             test_case "Closed fd exceptions" `Quick test_closed_fd;
           ] );
       ]
