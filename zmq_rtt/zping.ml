open Zmq_lwt

let rec retry f = 
  try%lwt
    f ()
  with Unix.Unix_error (EHOSTUNREACH,_,_) -> 
    let%lwt () = Lwt_unix.sleep 1. in
    print_endline "retrying"; retry f

let send_all sock msg = 
  retry (fun () -> Socket.send_all sock msg)

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
        let sock = Zmq.Socket.create ctx Zmq.Socket.router in
        Zmq.Socket.set_identity sock "ping";
        Zmq.Socket.set_router_mandatory sock true;
        Zmq.Socket.set_probe_router sock true;
        Zmq.Socket.connect sock loc;
        let sock = Socket.of_socket sock in
        print_endline "sending ping";
        let _ = Lwt_main.run @@
          let p = send_all sock ["not_a_real_destination"; "ping"] in
          let q = 
            let%lwt () = send_all sock ["pong"; "ping"; "ping"]in
            let%lwt resp = Socket.recv sock in
            Lwt.return @@ print_endline resp
          in Lwt.join[p;q]
        in
        Zmq.Context.terminate ctx
    )

let () = Core.Command.run command
