open Zmq_lwt

let command = 
  Core.Command.basic
    ~summary:""
    Core.Command.Let_syntax.(
      let%map_open loc = anon ("loc" %: string)
      in
      fun () -> 
        print_endline "Pinging";
        print_endline @@ Printf.sprintf "Connecting to :%s " loc;
        let ctx = Zmq.Context.create () in
        let sock = 
          Zmq.Socket.create ctx Zmq.Socket.req in
        let () = Zmq.Socket.connect sock loc in
        let sock = Socket.of_socket sock in
        print_endline "sending ping";

        Lwt_main.run @@
        let t = Sys.time() in
        let%lwt () = Socket.send sock @@ Printf.sprintf "%f" @@Sys.time() in
        let%lwt resp = Socket.recv sock in
        Lwt.return @@ Printf.printf "Received \"%s\" in %f " resp (Sys.time() -. t) 
    )

let () = Core.Command.run command
