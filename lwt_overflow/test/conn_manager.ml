open Unix_capnp_messaging
open Conn_manager
open Lwt.Infix

let ( >>>= ) = Lwt_result.bind

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

let addr i = TCP ("127.0.0.1", 5000 + i)

let mgr i handler =
  create ~listen_address:(addr i) ~node_id:(Int64.of_int i) handler

let echo_mgr i = mgr i (send ~semantics:`AtLeastOnce)

let timeout t f s =
  let p = f () >>= Lwt.return_ok in
  let t = Lwt_unix.sleep t >>= Lwt.return_error in
  Lwt.choose [ p; t ] >>= function
  | Ok v -> Lwt.return v
  | Error () -> Alcotest.fail s

let test_one_way () =
  let p, f = Lwt.task () in
  let m1 =
    mgr 1 (fun _t src msg ->
        Lwt.wakeup f (src, msg);
        Lwt.return_ok ())
  in
  let m2 = mgr 2 (fun _ _ _ -> Lwt.return_ok ()) in
  add_outgoing m2 Int64.one (addr 1) (`Persistant (addr 1)) >>= fun () ->
  send ~semantics:`AtLeastOnce m2 Int64.one (of_store test_message) >>= function
  | Error exn -> Alcotest.fail (Fmt.str "Sending failed with %a" Fmt.exn exn)
  | Ok () ->
      p >>= fun (src, msg) ->
      Alcotest.(check string)
        "Received message"
        (msg |> to_store |> Bytes.to_string)
        (test_message |> Bytes.to_string);
      Alcotest.(check int64) "Received message" src (Int64.of_int 2);
      close m1 >>= fun () -> close m2

let test_loop () =
  let m1 = echo_mgr 1 in
  let p, f = Lwt.task () in
  let m2 =
    mgr 2 (fun _ src msg ->
        Lwt.wakeup f (src, msg);
        Lwt.return_ok ())
  in
  add_outgoing m2 Int64.one (addr 1) (`Persistant (addr 1)) >>= fun () ->
  send ~semantics:`AtLeastOnce m2 Int64.one (of_store test_message) >>= function
  | Error exn -> Alcotest.fail (Fmt.str "Sending failed with %a" Fmt.exn exn)
  | Ok () ->
      p >>= fun (src, msg) ->
      Alcotest.(check string)
        "Received message"
        (msg |> to_store |> Bytes.to_string)
        (test_message |> Bytes.to_string);
      Alcotest.(check int64) "Received source" src Int64.one;
      close m1 >>= fun () -> close m2

(* Two connections, one of which breaks and the other should still function *)
let test_conn_failure () =
  let main () =
    let p, f = Lwt.task () in
    let m1 =
      mgr 1 (fun _ src _msg ->
          Lwt.wakeup f src;
          Lwt.return_ok ())
    in
    let m2 = mgr 2 (fun _ _ _ -> Lwt.return_ok ()) in
    let m3 = mgr 3 (fun _ _ _ -> Lwt.return_ok ()) in
    Lwt.join
      [
        add_outgoing m2 Int64.one (addr 1) (`Persistant (addr 1));
        add_outgoing m3 Int64.one (addr 1) (`Persistant (addr 1));
      ]
    >>= fun () ->
    close m3 >>= fun () ->
    send ~semantics:`AtLeastOnce m2 Int64.one (of_store test_message)
    >>>= fun () ->
    p >>= fun src ->
    Alcotest.(check int64) "Received source" src (Int64.of_int 2);
    Lwt.join [ close m1; close m2 ] >>= Lwt.return_ok
  in
  main () >>= function
  | Error exn -> Alcotest.fail (Fmt.str "Sending failed with %a" Fmt.exn exn)
  | Ok () -> Lwt.return_unit

(* Two connections, one of which breaks and the other should still be able to connect *)
let test_conn_failure_reconnect () =
  let main () =
    let p, f = Lwt.task () in
    let m1 =
      mgr 1 (fun _ src _msg ->
          Logs.debug (fun m -> m "Got request from %a" Fmt.int64 src);
          if src = Int64.of_int 2 then Lwt.wakeup f src;
          Lwt.return_ok ())
    in
    let m2 = mgr 2 (fun _ _ _ -> Lwt.return_ok ()) in
    let m3 = mgr 3 (fun _ _ _ -> Lwt.return_ok ()) in
    add_outgoing m3 Int64.one (addr 1) (`Persistant (addr 1)) >>= fun () ->
    send ~semantics:`AtLeastOnce m3 Int64.one (of_store test_message) >>>= fun () ->
    close m3 >>= fun () ->
    add_outgoing m2 Int64.one (addr 1) (`Persistant (addr 1)) >>= fun () ->
    send ~semantics:`AtLeastOnce m2 Int64.one (of_store test_message)
    >>>= fun () ->
    p >>= fun src ->
    Alcotest.(check int64) "Received source" src (Int64.of_int 2);
    Lwt.join [ close m1; close m2 ] >>= Lwt.return_ok
  in
  main () >>= function
  | Error exn -> Alcotest.fail (Fmt.str "Sending failed with %a" Fmt.exn exn)
  | Ok () -> Lwt.return_unit

(* Throw an exception on one of the receives and the other should succeed *)
let test_recv_exception () =
  let main () =
    let p, f = Lwt.task () in
    let m1 =
      mgr 1 (fun _ src _msg ->
          if Int64.(src = of_int 2) then raise Not_found else Lwt.wakeup f src;
          Lwt.return_ok ())
    in
    let m2 = mgr 2 (fun _ _ _ -> Lwt.return_ok ()) in
    let m3 = mgr 3 (fun _ _ _ -> Lwt.return_ok ()) in
    Lwt.join
      [
        add_outgoing m2 Int64.one (addr 1) (`Persistant (addr 1));
        add_outgoing m3 Int64.one (addr 1) (`Persistant (addr 1));
      ]
    >>= fun () ->
    send ~semantics:`AtLeastOnce m2 Int64.one (of_store test_message)
    >>>= fun () ->
    send ~semantics:`AtLeastOnce m3 Int64.one (of_store test_message)
    >>>= fun () ->
    p >>= fun src ->
    Alcotest.(check int64) "Received source" (Int64.of_int 3) src;
    Lwt.join [ close m1; close m2 ] >>= Lwt.return_ok
  in
  main () >>= function
  | Error exn -> Alcotest.fail (Fmt.str "Sending failed with %a" Fmt.exn exn)
  | Ok () -> Lwt.return_unit

let stream_mgr i =
  let stream, push = Lwt_stream.create () in
  (mgr i (fun _ src msg -> push (Some (src, msg)) |> Lwt.return_ok), stream)

let test_mutual () =
  let m1, str1 = stream_mgr 1 in
  let m2, str2 = stream_mgr 2 in
  add_outgoing m1 (Int64.of_int 2) (addr 2) (`Persistant (addr 2)) >>= fun () ->
  send ~semantics:`AtLeastOnce m2 Int64.one (of_store test_message) >>= function
  | Error exn ->
      Alcotest.fail (Fmt.str "Initial message failed with %a" Fmt.exn exn)
  | Ok () -> (
      Lwt_stream.next str1 >>= fun _ ->
      Logs.debug (fun m -> m "Sent and received from m2 -> m1");
      Logs.debug (fun m -> m "Reordering connection");
      add_outgoing m2 Int64.one (addr 1) (`Persistant (addr 1)) >>= fun () ->
      Lwt_unix.sleep 1. >>= fun () ->
      let sends =
        send ~semantics:`AtLeastOnce m2 Int64.one (of_store test_message)
        >>>= fun () ->
        send ~semantics:`AtLeastOnce m1 (Int64.of_int 2) (of_store test_message)
      in
      sends >>= function
      | Error exn ->
          Alcotest.fail
            (Fmt.str "Failed to send reinit messages %a" Fmt.exn exn)
      | Ok () ->
          Lwt_stream.next str1 >>= fun _ ->
          Lwt_stream.next str2 >>= fun _ -> Lwt.join [ close m1; close m2 ] )

(* Throw an exception on one of the receives and the other should succeed *)
let test_recv_sleep () =
  let main () =
    let p, f = Lwt.task () in
    let m1 =
      mgr 1 (fun _ _src msg ->
          let msg = to_store msg in
          Alcotest.(check int) "Buffer size" 16 (Bytes.length msg);
          if Bytes.get_int16_le msg 0 = 0 then
            Lwt_unix.sleep 100. >>= Lwt.return_ok
          else (
            Lwt.wakeup f (Bytes.get_int16_le msg 0);
            Lwt.return_ok () ))
    in
    let fail_msg =
      let buf = Bytes.create 16 in
      Bytes.set_int16_be buf 0 0;
      of_store buf
    in
    let success_i = 25 in
    let succeed_msg =
      let buf = Bytes.create 16 in
      Bytes.set_int16_le buf 0 success_i;
      of_store buf
    in
    let m2 = mgr 2 (fun _ _ _ -> Lwt.return_ok ()) in
    add_outgoing m2 Int64.one (addr 1) (`Persistant (addr 1)) >>= fun () ->
    send ~semantics:`AtLeastOnce m2 Int64.one fail_msg >>>= fun () ->
    send ~semantics:`AtLeastOnce m2 Int64.one succeed_msg >>>= fun () ->
    p >>= fun src ->
    Alcotest.(check int) "Received message" success_i src;
    Lwt.join [ close m1; close m2 ] >>= Lwt.return_ok
  in
  main () >>= function
  | Error exn -> Alcotest.fail (Fmt.str "Sending failed with %a" Fmt.exn exn)
  | Ok () -> Lwt.return_unit

let test_wrapper f _ () = timeout 5. f "Timed out"

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
  let debug = false in
  if debug then Lwt_main.run (test_mutual ())
  else
    let open Alcotest_lwt in
    Lwt_main.run
    @@ run "messaging layer"
         [
           ( "Basic",
             [
               test_case "One Way" `Quick (test_wrapper test_one_way);
               test_case "Loopback" `Quick (test_wrapper test_loop);
               test_case "Mutual" `Quick (test_wrapper test_mutual);
             ] );
           ( "Failure",
             [
               test_case "Connection failure" `Quick
                 (test_wrapper test_conn_failure);
               test_case "Connection failure reconnect" `Quick
                 (test_wrapper test_conn_failure_reconnect);
               test_case "Exception handler" `Quick
                 (test_wrapper test_recv_exception);
               test_case "Sleep handler" `Quick (test_wrapper test_recv_sleep);
             ] );
         ]
