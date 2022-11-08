let () = print_endline ""

open! Core
open! Eio_rpc

let traceln = Eio.traceln

module Switch = Eio.Switch
module Net = Eio.Net
module Flow = Eio.Flow

module Server = struct
  let main ~sw socket : unit =
    let rec loop () =
      let callback flow addr =
        traceln "Accepted connection from %a" Eio.Net.Sockaddr.pp addr;
        Rpc.Connection.with_connection flow (fun c ->
            let rec loop () =
              if Rpc.Connection.is_closed c then
                traceln "Finished connection %a" Eio.Net.Sockaddr.pp addr
              else
                let pkt = Rpc.Connection.recv c in
                Rpc.Connection.send c pkt;
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
        main ~sw socket)
end

module MBar = struct
  type t = {
    limit : int;
    mutable current : int;
    mtx : Eio.Mutex.t;
    cond : Eio.Condition.t;
  }

  let create n =
    {
      limit = n;
      current = 0;
      mtx = Eio.Mutex.create ();
      cond = Eio.Condition.create ();
    }

  let signal t =
    Eio.Mutex.lock t.mtx;
    t.current <- t.current + 1;
    if t.current >= t.limit then Eio.Condition.broadcast t.cond;
    Eio.Mutex.unlock t.mtx

  let await t =
    Eio.Mutex.lock t.mtx;
    while t.current < t.limit do
      Eio.Condition.await t.cond t.mtx
    done;
    Eio.Mutex.unlock t.mtx
end

module Client = struct
  let process txs rxs =
    let start =
      txs |> Array.filter_opt
      |> Array.min_elt ~compare:Float.compare
      |> Option.value_exn
    in
    let stop =
      rxs |> Array.filter_opt
      |> Array.max_elt ~compare:Float.compare
      |> Option.value_exn
    in
    let delay_times =
      Array.zip_exn txs rxs
      |> Array.filter_map ~f:(function
           | Some tx, Some rx -> Some Float.(rx - tx)
           | _ -> None)
    in
    Array.sort delay_times ~compare:Float.compare;
    let n = Array.length txs in
    traceln "Completed test";
    traceln "Rate = %.2f req/s" Float.(of_int n / (stop - start));
    let to_ms f = Float.(f * 1000.) in
    let mean_lat =
      to_ms @@ Float.(Array.sum (module Float) delay_times ~f:Fn.id / of_int n)
    in
    let p000 = to_ms @@ Array.get delay_times 0 in
    let p050 = to_ms @@ Array.get delay_times Int.(n / 2) in
    let p099 = to_ms @@ Array.get delay_times Int.(99 * (n / 100)) in
    let p100 = to_ms @@ Array.get delay_times (n - 1) in
    traceln "Latency [0:%.3f, 50:%.3f, 99:%.3f, 100:%.3f, mean:%.3f]" p000 p050
      p099 p100 mean_lat

  let main ~sw ?(concurrency = 1) (socket : #Flow.two_way) n clock : unit =
    Rpc.Connection.with_connection socket @@ fun c ->
    let rpc_s = Rpc.RpcService.create ~sw c in
    let send_times = Array.create ~len:n None in
    let recv_times = Array.create ~len:n None in
    let open Eio in
    let sem = Semaphore.make concurrency in
    let mbar = MBar.create n in
    let rec fork_sender idx =
      if idx = n then ()
      else (
        Eio.Fiber.fork ~sw (fun () ->
            Semaphore.acquire sem;
            Array.set send_times idx @@ Some (Time.now clock);
            Rpc.RpcService.issue rpc_s
              (Cstruct.of_string @@ Fmt.str "Payload %d" n)
            |> Promise.await |> ignore;
            Array.set recv_times idx @@ Some (Time.now clock);
            Semaphore.release sem;
            MBar.signal mbar);
        fork_sender (idx + 1))
    in
    fork_sender 0;
    MBar.await mbar;
    process send_times recv_times

  let run n port concurrency : unit =
    Eio_main.run @@ fun env ->
    let net = Eio.Stdenv.net env in
    let clock = Eio.Stdenv.clock env in
    let addr = `Tcp (Net.Ipaddr.V4.loopback, port) in
    [%message (n : int) (port : int)] |> print_s;
    Switch.run @@ fun sw ->
    let socket = Net.connect ~sw net addr in
    main ~sw ~concurrency socket n clock;
    traceln "completed test";
    Net.close socket
end

let () =
  let open Cmdliner in
  let port_t = Arg.(required & pos 0 (some int) None & info ~docv:"PORT" []) in
  let server_t = Term.(const Server.run $ port_t) in
  let n_t =
    Arg.(required & pos 1 (some int) None & info ~docv:"ITERATIONS" [])
  in
  let concurrency_t =
    Arg.(required & pos 2 (some int) None & info ~docv:"CONCURRENCY" [])
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
