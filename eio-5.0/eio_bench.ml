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
    let header = Cstruct.create_unsafe sizeof_packet_header in
    (* TODO test using built in serialisers *)
    set_packet_header_id header p.id;
    set_packet_header_length header p.payload.len;
    schedule_cstruct w header;
    schedule_cstruct w p.payload
end

module Server = struct
  let main socket : unit =
    Switch.run @@ fun sw ->
    let rec loop () =
      let callback flow addr =
        traceln "Accepted connection from %a" Eio.Net.Sockaddr.pp addr;
        let buf_read = Eio.Buf_read.of_flow ~max_size:1_000_000 flow in
        Eio.Buf_write.with_flow flow (fun writer ->
            let rec loop () =
              if Eio.Buf_read.at_end_of_input buf_read then
                traceln "Finished connection %a" Eio.Net.Sockaddr.pp addr
              else
                let packet = LineProtocol.packet buf_read in
                LineProtocol.write_packet packet writer;
                loop ()
            in
            loop ())
      in
      let on_error (e : exn) = traceln "Failed with %a" Fmt.exn e in
      Eio.Net.accept_fork socket ~sw ~on_error callback;
      loop ()
    in
    loop ()

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
  let main ?(concurrency = 1) (socket : #Flow.two_way) n clock : unit =
    let sem = Eio.Semaphore.make concurrency in
    let send_times = Array.create ~len:n None in
    let sender () =
      Eio.Buf_write.with_flow socket (fun writer ->
          let rec loop n =
            match n with
            | 0 -> ()
            | _ ->
                Eio.Semaphore.acquire sem;
                let packet =
                  LineProtocol.
                    {
                      id = Int32.of_int_exn n;
                      payload = Cstruct.of_string (Fmt.str "Payload %d" n);
                    }
                in
                let idx = n - 1 in
                assert (Array.get send_times idx |> Option.is_none);
                Array.set send_times idx @@ Some (Eio.Time.now clock);
                LineProtocol.write_packet packet writer;
                loop (n - 1)
          in
          loop n)
    in
    let recv_times = Array.create ~len:n None in
    let receiver () =
      let open Eio.Buf_read in
      let buf_read = of_flow ~max_size:1_000_000 socket in
      let rec loop acc =
        if Int.(acc = n) || at_end_of_input buf_read then acc
        else
          let packet = LineProtocol.packet buf_read in
          let idx = Int32.to_int_exn packet.id - 1 in
          assert (Array.get recv_times idx |> Option.is_none);
          Array.set recv_times idx (Some (Eio.Time.now clock));
          Eio.Semaphore.release sem;
          loop (acc + 1)
      in
      let received_packets = loop 0 in
      traceln "received %d packets" received_packets
    in
    Eio.Fiber.both sender receiver;
    let start =
      send_times |> Array.filter_opt
      |> Array.min_elt ~compare:Float.compare
      |> Option.value_exn
    in
    let stop =
      recv_times |> Array.filter_opt
      |> Array.max_elt ~compare:Float.compare
      |> Option.value_exn
    in
    let delay_times =
      Array.zip_exn send_times recv_times
      |> Array.filter_map ~f:(function
           | Some tx, Some rx -> Some Float.(rx - tx)
           | _ -> None)
    in
    traceln "Completed test";
    traceln "Rate = %.2f req/s" Float.(of_int n / (stop - start));
    let mean_lat =
      Float.(Array.sum (module Float) delay_times ~f:Fn.id / of_int n)
    in
    let delta_arr =
      Array.map delay_times
        ~f:Float.(fun lat -> (lat - mean_lat) * (lat - mean_lat) / of_int n)
    in
    let sd = Float.sqrt @@ Array.sum (module Float) delta_arr ~f:Fn.id in
    Float.(traceln "Latency ~N(%.3f %.3f) ms" (mean_lat * 1000.) (sd * 1000.))

  let run n port concurrency : unit =
    Eio_main.run @@ fun env ->
    let net = Eio.Stdenv.net env in
    let clock = Eio.Stdenv.clock env in
    let addr = `Tcp (Net.Ipaddr.V4.loopback, port) in
    [%message (n : int) (port : int)] |> print_s;
    Switch.run @@ fun sw ->
    let socket = Net.connect ~sw net addr in
    main ~concurrency socket n clock;
    Net.close socket
end

let () =
  let open Cmdliner in
  let port_t =
    Arg.(required & pos 0 (some int) None & info ~docv:"Port number" [])
  in
  let server_t = Term.(const Server.run $ port_t) in
  let n_t =
    Arg.(
      required & pos 1 (some int) None & info ~docv:"number of iterations" [])
  in
  let concurrency_t =
    Arg.(
      required & pos 2 (some int) None & info ~docv:"internal concurrency" [])
  in
  let client_t = Term.(const Client.run $ n_t $ port_t $ concurrency_t) in
  let info_i =
    Cmd.info "eio_bench" ~doc:"Tests the performance of an echo server for eio"
  in
  let open Cmd in
  let cmd =
    group info_i [ v (info "server") server_t; v (info "client") client_t ]
  in
  exit (Cmd.eval cmd)
