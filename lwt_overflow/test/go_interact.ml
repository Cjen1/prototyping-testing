module U = Unix_capnp_messaging
module UC = U.Conn_manager
module A = Protocol_api.Make [@inlined] (Capnp.BytesMessage)

open Lwt.Infix

let (>>>=) v f = v >>= function
  | Error _ -> Alcotest.fail "bind"
  | Ok v -> f v

let to_store msg = 
  let module R = A.Reader.EchoTest in
  msg |> R.of_message |> R.text_get

let of_store msg =
  let module B = A.Builder.EchoTest in
  let root = B.init_root () in
  B.text_set root msg;
  B.to_message root
  

let open_client switch =
  let command = Lwt_process.shell "go run client.go" in
  let proc = Lwt_process.(open_process_none ~stdout:`Keep ~stderr:`Keep command) in
  Lwt_switch.add_hook (Some switch) (fun () -> proc#kill Sys.sigterm |> Lwt.return)

let open_conn switch addr recv_handler =
  let conn_mgr = UC.create ~listen_address:addr ~node_id:Int64.one recv_handler in
  Lwt_switch.add_hook (Some switch) (fun () -> UC.close conn_mgr);
  conn_mgr

let test_interact switch () =
  let addr = UC.TCP ("127.0.0.1", 5000) in
  let ref_p = ref (Lwt.task ()) in
  let recv_handler _mgr src msg = 
    Lwt.wakeup_later (snd !ref_p) (src,msg) |> Lwt.return_ok
  in 
  let conn_mgr = open_conn switch addr recv_handler in
  open_client switch;
  fst !ref_p >>= fun (src, msg) ->
  ref_p := Lwt.task ();
  Alcotest.(check int64) "go_client id" (Int64.of_int 2) src;
  Alcotest.(check string) "Correct msg from client" "asdf" (msg |> to_store );
  Lwt_unix.sleep 1. >>= fun () ->
  let msg = "test_msg" |> of_store in
  UC.send ~semantics:`AtLeastOnce conn_mgr (Int64.of_int 2) msg >>>= fun () ->
  fst !ref_p >>= fun (src, msg) ->
  Alcotest.(check int64) "go_client id" (Int64.of_int 2) src;
  Alcotest.(check string) "Correct msg to client" "success" (msg |> to_store);
  Lwt.return_unit

let timeout f t switch () =
  let p_timeout = Lwt_unix.sleep t >>= fun () -> Lwt.return_none in
  let p_f = f switch () >>= Lwt.return_some in
  Lwt.choose [p_timeout; p_f] >>= function
  | None -> Alcotest.fail "Timed out"
  | Some v -> Lwt.return v

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
  let open Alcotest_lwt in
  Lwt_main.run
  @@ run "Go Interaction"
    [
      ("Test infra", [ test_case "Basic interaction" `Quick (timeout test_interact 10.)])
    ]
