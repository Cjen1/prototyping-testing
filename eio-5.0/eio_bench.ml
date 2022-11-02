let () = print_endline ""

open! Core
open! Eio_main
open! Eio

module Server = struct
  let main socket ~sw =
    Eio.Net.accept_fork socket ~sw
      (fun flow _addr ->
        traceln "Accepted connection";
        let b = Buffer.create 100 in
        Eio.Flow.copy flow (Eio.Flow.buffer_sink b);
        traceln "Server got %s" (Buffer.contents b);
      )
      ~on_error:(traceln "Error got: %a" Fmt.exn)

  let run port : unit =
    Eio_main.run @@ fun _env ->
    Switch.run @@ fun sw ->
    traceln "Listening on 127.0.0.1:%d" port;
    main _ ~sw
end

module Client = struct
  let main _socket = ()

  let run n port : unit =
    Eio_main.run @@ fun _env ->
    [%message (n : int) (port : int)] |> print_s;
    main _
end

let () =
  let open Cmdliner in
  let port_t = Arg.(required & pos 0 (some int) None & info []) in
  let server_t = Term.(const Server.run $ port_t) in
  let n_t = Arg.(required & pos 1 (some int) None & info []) in
  let client_t = Term.(const Client.run $ n_t $ port_t) in
  let info_i =
    Cmd.info "eio_bench" ~doc:"Tests the performance of an echo server for eio"
  in
  let open Cmd in
  let cmd =
    group info_i [ v (info "server") server_t; v (info "client") client_t ]
  in
  exit (Cmd.eval cmd)
