open Zmq_lwt

let command = 
  fun () -> 
  let loc = "tcp://0.0.0.0:5001" in
  print_endline "Pongging";
  print_endline @@ Printf.sprintf "Binding to %s" loc;
  let ctx = Zmq.Context.create () in
  let sock = 
    Zmq.Socket.create ctx Zmq.Socket.router in
  Zmq.Socket.set_identity sock "pong";
  Zmq.Socket.set_probe_router sock true;
  Zmq.Socket.set_router_mandatory sock true;
  let () = Zmq.Socket.bind sock loc in
  let sock = Socket.of_socket sock in
  Lwt_main.run @@
  let%lwt resp = Socket.recv_all sock in
  let addr, resp = match resp with
  | [addr; resp] -> addr, resp
  | _ -> assert false
  in
  print_endline resp;
  let%lwt () = Socket.send_all sock [addr;"pong"] in
  Lwt.return_unit

let () = command ()
