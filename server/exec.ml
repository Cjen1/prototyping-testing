(*module Serv = Server.Make_Server (struct
    type nonrec t = unit

    let connected_callback (ic,oc) (_:t) = 
      let%lwt msg = Lwt_io.read_line ic in
      print_endline msg;
      Lwt_io.write_line oc msg
  end)
  *)

let handle socket _ () =
    let ic = Lwt_io.of_fd ~mode:Lwt_io.Input (socket) in
    let oc = Lwt_io.of_fd ~mode:Lwt_io.Output (socket) in
    let%lwt msg = Lwt_io.read_line ic in
    print_endline msg;
    let%lwt () = Lwt_io.write_line oc msg in
    Lwt_unix.shutdown socket Lwt_unix.SHUTDOWN_ALL;
    Lwt_unix.close socket
    (*Lwt_unix.close socket*)

let server () = 
  let open Lwt_unix in
  let sock = socket PF_INET SOCK_STREAM 0 in
  setsockopt sock SO_REUSEADDR true ;
  let%lwt () = bind sock @@ ADDR_INET (Unix.inet_addr_of_string "127.0.0.1", 5001) in
  print_endline "bound";
  Lwt_unix.listen sock 10;
  print_endline "listening";
  let rec serve () = 
    print_endline "accepting";
    let%lwt (client, _) = Lwt_unix.accept sock in
    assert (client <> sock);
    let ic = Lwt_io.of_fd ~mode:Lwt_io.Input (client) in
    let oc = Lwt_io.of_fd ~mode:Lwt_io.Output (client) in
    let p = 
      let%lwt msg = Lwt_io.read_line ic in
      print_endline msg;
      let%lwt () = Lwt_io.write_line oc msg in
      close client 
    in
    Lwt.join[p; serve ()]
  in serve ()

let server' () =
  let open Lwt_unix in
  let sock = socket PF_INET SOCK_STREAM 0 in
  setsockopt sock SO_REUSEADDR true ;
  let%lwt () = bind sock @@ ADDR_INET (Unix.inet_addr_of_string "127.0.0.1", 5001) in
  print_endline "bound";
  Lwt_unix.listen sock 10;
  print_endline "listening";
  let rec serve () =
    print_endline "accepting";
    let%lwt (client, addr) = Lwt_unix.accept sock in
    Lwt.async(handle client addr);
    serve ()
  in serve()

let cat () =
  let addr = Unix.inet_addr_of_string "127.0.0.1" in
  print_endline "Spooling up server";
  Server.start addr 5001 () (fun (read, write) () ->
    let%lwt recv = read () in
    print_endline recv;
    let%lwt () = write recv in
    print_endline "Sent";
    Lwt.return_unit
  )

    



let () = 
  Lwt_engine.set (new Lwt_engine.libev ());
  Lwt_main.run @@cat ()
