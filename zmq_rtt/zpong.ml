open Zmq_lwt

let send socket msg = 
  Socket.send socket msg

let command = 
  Core.Command.basic
    ~summary:""
    Core.Command.Let_syntax.(
      let%map_open ping = anon ("path" %: string)
      and          pong = anon ("path" %: string)
      in
      fun () -> 
        Lwt_main.run @@
        let ctx = Zmq.Context.create () in
        let ping_socket = 
          Zmq.Socket.create ctx Zmq.Socket.sub in
        let () = Zmq.Socket.connect ping_socket ping in
        let pong_socket = 
          Zmq.Socket.create ctx Zmq.Socket.pub in
        let () = Zmq.Socket.bind pong_socket pong in
        let ping_socket = Socket.of_socket ping_socket in
        let pong_socket = Socket.of_socket pong_socket in
        let t = Sys.time() in
        let%lwt () = send ping_socket "pong" in
        let%lwt resp = Socket.recv pong_socket in
        Lwt.return @@ Printf.printf "Received \"%s\" in %f " resp (Sys.time() -. t) 
    )

let () = Core.Command.run command
