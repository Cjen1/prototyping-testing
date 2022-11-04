let () = print_endline ""

open! Core

let traceln = Eio.traceln

module Switch = Eio.Switch
module Net = Eio.Net
module Flow = Eio.Flow

module LineProtocol = struct
  open! Eio.Buf_read
  open! Eio.Buf_read.Syntax

  type packet_header = Cstruct.t

  [%%cstruct
  type packet_header = { id : uint32_t; length : uint16_t } [@@big_endian]]

  let header : packet_header parser =
    let+ b = take sizeof_packet_header in
    Cstruct.of_string b

  type packet = { id : int32; payload : Cstruct.t }

  let packet : packet parser =
    let* header = header in
    let* payload =
      let+ s = take (get_packet_header_length header) in
      Cstruct.of_string s
    in
    return { payload; id = get_packet_header_id header }

  let write_packet p w =
    let open Eio.Buf_write in
    let header = Cstruct.create_unsafe 3 in
    (* TODO test using built in serialisers *)
    set_packet_header_id header p.id;
    set_packet_header_length header p.payload.len;
    schedule_cstruct w header;
    schedule_cstruct w p.payload
end

module Server = struct
  let main socket : unit =
    Switch.run @@ fun sw ->
    let callback flow addr =
      traceln "Accepted connection from %a" Eio.Net.Sockaddr.pp addr;
      let buf_read = Eio.Buf_read.of_flow ~max_size:1_000_000 flow in
      Eio.Buf_write.with_flow flow (fun writer ->
          let rec loop () =
            let packet = LineProtocol.packet buf_read in
            traceln "Server recv'd %d: \"%s\""
              Int32.(to_int_exn packet.id)
              (Cstruct.to_string packet.payload);
            LineProtocol.write_packet packet writer;
            if Int32.(packet.id = zero) then traceln "Finished connection"
            else loop ()
          in
          loop ())
    in
    let on_error (e : exn) = traceln "Failed with %a" Fmt.exn e in
    Eio.Net.accept_fork socket ~sw ~on_error callback

  let run port : unit =
    Eio_main.run @@ fun env ->
    let net = Eio.Stdenv.net env in
    let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, port) in
    traceln "Listening on 127.0.0.1:%d" port;
    Switch.run (fun sw ->
        let socket = Eio.Net.listen ~sw net addr ~backlog:5 in
        main socket)
end

module Client = struct
  let main socket n : unit =
    let sender () =
      Eio.Buf_write.with_flow socket (fun writer ->
          let rec loop n =
            match n with
            | 0 -> ()
            | _ ->
                let packet =
                  LineProtocol.
                    {
                      id = Int32.of_int_exn n;
                      payload = Cstruct.of_string (Fmt.str "Payload %d" n);
                    }
                in
                LineProtocol.write_packet packet writer;
                loop (n - 1)
          in
          loop n)
    in
    let receiver () =
      let open Eio.Buf_read in
      let seq_parse = seq LineProtocol.packet ~stop:at_end_of_input in
      match parse ~max_size:1_000_000_000 seq_parse socket with
      | Error (`Msg s) -> traceln "Receiver error'd out with: %s" s
      | Ok (xs : LineProtocol.packet Seq.t) ->
          let all =
            Seq.fold_left
              (fun acc x -> Cstruct.to_string x.LineProtocol.payload :: acc)
              [] xs
            |> List.rev
          in
          traceln "received %d" (List.length all);
          traceln "%a" Fmt.(list string) all
    in
    Eio.Fiber.both sender receiver

  let run n port : unit =
    Eio_main.run @@ fun env ->
    let net = Eio.Stdenv.net env in
    let addr = `Tcp (Net.Ipaddr.V4.loopback, port) in
    [%message (n : int) (port : int)] |> print_s;
    Switch.run @@ fun sw ->
    let socket = Net.connect ~sw net addr in
    main socket n;
    Net.close socket
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
