open Zmq_lwt
open Core

let command = 
  Command.basic
    ~summary:""
    Command.Let_syntax.(
      let%map_open r_pub = anon ("pub" %: string)
      and r_sub = anon ("sub" %: string)
      in
      fun () -> 
        let ctx = Zmq.Context.create () in
        let pub = Zmq.Socket.create ctx Zmq.Socket.pub in
        let sub = Zmq.Socket.create ctx Zmq.Socket.sub in
        let () = Zmq.Socket.connect pub r_sub in
        let () = Zmq.Socket.connect sub r_pub in
        print_endline "sending ping";
        let pub = Socket.of_socket ctx Zmq.Socket.pub in
        let sub = Socket.of_socket ctx Zmq.Socket.sub in

        Zmq.Socket.subscribe sub ""

        Lwt_main.run @@ 
        let%lwt () = Socket.send_all pub ["filt"; "ab"; "cd"] in
        let%lwt resp = Socket.recv_all sock in
        (match resp with 
        | [a;b] -> Stdlib.print_endline @@ Sprintf.printf "a=%s\n b=%s" a b
        | xs -> print_endline @@ Sprintf.printf "Not recognised: %s" @@ List.to_string xs
        ); Lwt.return_unit 


let () = Command.run command
