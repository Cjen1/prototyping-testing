open! Core 
open! Async
open! Intf

let implementations = 
  [ Rpc.Rpc.implement echo_api (fun () msg ->
        let%bind () = after (Time.Span.of_sec 1.) in
        return ("echo" ^ msg))
  ]

let server ~port =
  print_endline @@ Fmt.str "Starting server on port %d\n" port;
  let implementations =
    Rpc.Implementations.create ~implementations ~on_unknown_rpc:`Continue
  in
  match implementations with
  | Error (`Duplicate_implementations _descrs) -> assert false
  | Ok implementations ->
    let server =
      Tcp.Server.create
        (Tcp.Where_to_listen.of_port port)
        ~on_handler_error:`Ignore
        (fun _addr reader writer ->
           Rpc.Connection.server_with_close
             reader
             writer
             ~implementations
             ~connection_state:(fun _ -> ())
             ~on_handshake_error:`Ignore)
    in
    ignore (server : (_, _) Tcp.Server.t Deferred.t);
    Deferred.never ()

let () =
  Command.async_spec
    ~summary:"A trivial Async-RPC server"
    Command.Spec.(
      empty 
      +> flag "-port" ~doc:" Port to listen on" (optional_with_default 8080 int)
    )
    (fun port () -> 
         server ~port
    )
  |> Command.run
;;
