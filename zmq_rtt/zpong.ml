open Zmq_lwt

let send socket msg = 
  Socket.send socket msg

let command = 
  fun () -> 
  let loc = "tcp://0.0.0.0:5001" in
  print_endline "Pongging";
  print_endline @@ Printf.sprintf "Binding to %s" loc;
  let ctx = Zmq.Context.create () in
  let sock = 
    Zmq.Socket.create ctx Zmq.Socket.rep in
  let () = Zmq.Socket.bind sock loc in
  let sock = Socket.of_socket sock in

  Lwt_main.run @@
  let t = Sys.time() in
  let%lwt resp = Socket.recv sock in
  let resp = Sys.time() -. (Float.of_string resp) in
  let%lwt () = send sock "pong" in
  Lwt.return @@ Printf.printf "Received \"ping: %f\" in %f " resp (Sys.time() -. t) 

let () = command ()
