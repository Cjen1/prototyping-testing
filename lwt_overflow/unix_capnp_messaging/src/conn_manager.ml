open Lwt.Infix
module U = Utils

let ( >>>= ) a b = Lwt_result.bind a b

let src = Logs.Src.create "ConnMgr" ~doc:"Connection manager"

module Log = (val Logs.src_log src : Logs.LOG)

type address = Unix of string | TCP of (string * int)

let addr_of_string s =
  match String.split_on_char ':' s with
  | [ addr ] -> Ok (Unix addr)
  | [ addr; port ] -> (
      match int_of_string_opt port with
      | Some port -> Ok (TCP (addr, port))
      | None -> Error (`Msg (Fmt.str "Port (%s) is not a valid int" port)) )
  | _ -> Error (`Msg (Fmt.str "Expected address of the form ip:port, not %s" s))

let pp_addr f = function
  | Unix s -> Fmt.pf f "%s" s
  | TCP (s, p) -> Fmt.pf f "%s:%d" s p

module Utils = struct
  let addr_of_host host =
    match Unix.gethostbyname host with
    | exception Not_found -> Fmt.failwith "Unknown host %S" host
    | addr ->
        if Array.length addr.Unix.h_addr_list = 0 then
          Fmt.failwith "No addresses found for host name %S" host
        else addr.Unix.h_addr_list.(0)

  let connect_socket = function
    | Unix path ->
        Log.info (fun f -> f "Connecting to %S..." path);
        let socket =
          Unix.(socket PF_UNIX SOCK_STREAM 0) |> Lwt_unix.of_unix_file_descr
        in
        Lwt_unix.connect socket (Unix.ADDR_UNIX path) >|= fun () -> socket
    | TCP (host, port) ->
        Log.info (fun f -> f "Connecting to %s:%d..." host port);
        let socket = Unix.(socket PF_INET SOCK_STREAM 0) in
        Unix.setsockopt socket Unix.SO_KEEPALIVE true;
        let socket = Lwt_unix.of_unix_file_descr socket in
        Lwt_unix.connect socket (Unix.ADDR_INET (addr_of_host host, port))
        >|= fun () -> socket

  let bind_socket = function
    | Unix path ->
        Log.info (fun f -> f "Binding to %S..." path);
        ( match Unix.lstat path with
        | { Unix.st_kind = Unix.S_SOCK; _ } -> Unix.unlink path
        | _ -> ()
        | exception Unix.Unix_error (Unix.ENOENT, _, _) -> () );
        let socket = Unix.(socket PF_UNIX SOCK_STREAM 0) in
        let socket = socket |> Lwt_unix.of_unix_file_descr in
        Lwt_unix.bind socket (Unix.ADDR_UNIX path) >|= fun () -> socket
    | TCP (host, port) ->
        Log.info (fun f -> f "Binding to %s:%d..." host port);
        let socket = Unix.(socket PF_INET SOCK_STREAM 0) in
        Unix.setsockopt socket Unix.SO_REUSEADDR true;
        let socket = Lwt_unix.of_unix_file_descr socket in
        Lwt_unix.bind socket (Unix.ADDR_INET (addr_of_host host, port))
        >|= fun () -> socket

  let print_and_ignore src exn =
    Log.err (fun m -> m "Caught %a in %s" Fmt.exn exn src);
    Lwt.return_unit
end

type node_id = int64

type conn_kind = [ `Persistant of address | `Ephemeral ]

type conn_descr = {
  node_id : node_id;
  in_socket : Sockets.Incomming.t;
  out_socket : Sockets.Outgoing.t;
  switch : Lwt_switch.t;
  kind : conn_kind;
}

type recv_handler =
  t ->
  node_id ->
  Capnp.MessageSig.ro Capnp.BytesMessage.Message.t ->
  (unit, exn) Lwt_result.t

and t = {
  handler : recv_handler;
  mutable active_conns : (node_id * conn_descr) list;
  mutable persist_ids : (node_id * address) list;
  switch : Lwt_switch.t;
  switch_off_prom_fulfill : unit Lwt.t * unit Lwt.u;
  retry_connection_timeout : float;
  node_id : node_id;
}

exception EOF

let rec read_exactly fd buf offset len () =
  U.catch
    (fun () -> Lwt_unix.read fd buf offset len)
    (fun exn -> Lwt.fail exn)
  >>= function
  | 0 -> Lwt.fail EOF
  | read when read = len -> Lwt.return_ok ()
  | read -> (read_exactly [@tailcall]) fd buf (offset + read) (len - offset) ()

let get_id_from_sock sock =
  let buf = Bytes.create 8 in
  U.catch (read_exactly sock buf 0 8) Lwt.return_error >>>= fun () ->
  Bytes.get_int64_be buf 0 |> Lwt.return_ok

let recv_handler_wrapper t conn_descr () =
  let rec loop () =
    Sockets.Incomming.recv conn_descr.in_socket >>= function
    | Ok msg ->
        let p =
          U.catch
            (fun () -> t.handler t conn_descr.node_id msg)
            Lwt.return_error
          >|= function
          | Ok () -> ()
          | Error exn ->
              Log.err (fun m -> m "Handler failed with %a" Fmt.exn exn)
        in
        Lwt.async (fun () -> p);
        loop ()
    | Error Sockets.Closed ->
        Log.debug (fun m -> m "%a: Socket closed" Fmt.int64 t.node_id);
        Lwt.return_unit
    | Error exn ->
        Log.err (fun m -> m "Recv loop exited with exn: %a" Fmt.exn exn);
        Lwt.return_unit
  in
  loop ()

let conn_from_fd ?switch ?incomming ?outgoing fd id kind =
  let switch = match switch with Some v -> v | None -> Lwt_switch.create () in
  let ip =
    match incomming with
    | Some v -> Lwt.return v
    | None -> Sockets.Incomming.create ~switch fd
  in
  let op =
    match outgoing with
    | Some v -> Lwt.return v
    | None -> Sockets.Outgoing.create ~switch fd
  in
  Lwt.both ip op >|= fun (in_socket, out_socket) ->
  { node_id = id; in_socket; out_socket; switch; kind }

(* Adds connection to active conns *)
let rec add_conn t (conn_descr : conn_descr) =
  Lwt_switch.add_hook_or_exec (Some t.switch) (fun () ->
      Lwt_switch.turn_off conn_descr.switch)
  >>= fun () ->
  t.active_conns <- (conn_descr.node_id, conn_descr) :: t.active_conns;
  Log.debug (fun m ->
      m "%a: Added %a to active connections" Fmt.int64 t.node_id Fmt.int64
        conn_descr.node_id);
  Lwt.async (recv_handler_wrapper t conn_descr);
  match conn_descr.kind with
  | `Persistant addr ->
      if not @@ List.mem_assoc conn_descr.node_id t.persist_ids then
        t.persist_ids <- (conn_descr.node_id, addr) :: t.persist_ids;
      Lwt_switch.add_hook_or_exec (Some conn_descr.switch) (fun () ->
          add_outgoing t conn_descr.node_id addr conn_descr.kind)
  | _ -> Lwt.return_unit

(* Handles if there are two connections at once *)
and handle_new_conn t (new_conn : conn_descr) direction =
  Log.debug (fun m ->
      m "%a: Handling conn to %a" Fmt.int64 t.node_id Fmt.int64 t.node_id);
  let other_id = new_conn.node_id in
  let new_conn =
    match List.assoc_opt other_id t.persist_ids with
    | Some addr -> { new_conn with kind = `Persistant addr }
    | None -> new_conn
  in
  let old_prior () =
    Log.debug (fun m ->
        m "Preexisting conn from %a and we initiate => kill new" Fmt.int64
          other_id);
    U.catch
      (fun () -> Lwt_switch.turn_off new_conn.switch)
      (Utils.print_and_ignore "handle new conn")
  in
  let new_prior (existing_conn : conn_descr) =
    Log.debug (fun m ->
        m "Preexisting conn from %a and they initiate, => kill old" Fmt.int64
          other_id);
    t.active_conns <- List.remove_assoc other_id t.active_conns;
    add_conn t new_conn >>= fun () ->
    U.catch
      (fun () -> Lwt_switch.turn_off existing_conn.switch)
      (Utils.print_and_ignore "handle new conn with new_prior")
  in
  match (List.assoc_opt other_id t.active_conns, direction) with
  | Some _, `Incomming when t.node_id > other_id -> old_prior ()
  | Some _, `Outgoing when t.node_id < other_id -> old_prior ()
  | Some existing_conn, `Incomming -> new_prior existing_conn
  | Some existing_conn, `Outgoing -> new_prior existing_conn
  | None, _ ->
      Log.debug (fun m ->
          m "%a: Got novel connection to %a" Fmt.int64 t.node_id Fmt.int64
            other_id);
      add_conn t new_conn

and add_outgoing t id addr kind =
  let rec retry_loop () =
    let main =
      match Lwt_switch.is_on t.switch with
      | false -> Lwt.return_ok ()
      | true -> (
          let socket_switch = Lwt_switch.create () in
          let setup_socket =
            Log.info (fun m ->
                m "%a: Attempting to connect to %a" Fmt.int64 t.node_id pp_addr
                  addr);
            let connect () = Utils.connect_socket addr >>= Lwt.return_ok in
            U.catch connect Lwt.return_error >>>= fun socket ->
            Lwt_switch.add_hook (Some socket_switch) (fun () ->
                Log.debug (fun m -> m "Closing outgoing socket");
                Lwt_unix.close socket);
            let buf = Bytes.create 8 in
            Bytes.set_int64_be buf 0 t.node_id;
            let buf = Lwt_bytes.of_bytes buf in
            Sockets.send (Lwt_bytes.write socket) buf 0 8 >>>= fun () ->
            conn_from_fd socket ~switch:socket_switch id kind >>= fun descr ->
            handle_new_conn t descr `Outgoing >>= Lwt.return_ok
          in
          setup_socket >>= function
          | Ok () -> Lwt.return_ok ()
          | Error exn ->
              Lwt_switch.turn_off socket_switch >>= fun () ->
              Lwt.return_error exn )
    in
    main >>= function
    | _ when not (Lwt_switch.is_on t.switch) ->
        Log.debug (fun m ->
            m "%a: Retry loop stopped because manager is closed" Fmt.int64
              t.node_id);
        Lwt.return_unit
    | Ok () -> Lwt.return_unit
    | Error exn -> (
        Log.err (fun m -> m "Failed while connecting: %a" Fmt.exn exn);
        match kind with
        | `Persistant _ ->
            Lwt_unix.sleep t.retry_connection_timeout >>= fun () ->
            retry_loop ()
        | `Ephemeral -> Lwt.return_unit )
  in
  retry_loop ()

let accept_incomming_loop t addr () =
  let bound_switch = Lwt_switch.create () in
  let main () =
    let create_sock () =
      Utils.bind_socket addr >>= fun fd ->
      Lwt_switch.add_hook (Some bound_switch) (fun () -> 
          Log.debug (fun m -> m "Closing bound switch");
          Lwt_unix.close fd);
      Lwt_unix.listen fd 128;
      Lwt.return_ok fd
    in
    U.catch create_sock (fun exn -> Lwt.return_error (`Exn (`Create, exn)))
    >>>= fun bound_socket ->
    Lwt_switch.add_hook_or_exec (Some t.switch) (fun () ->
        Lwt_switch.turn_off bound_switch)
    >>= fun () ->
    let rec accept_loop () =
      let client_switch = Lwt_switch.create () in
      Log.debug (fun m -> m "Trying to accept new conn");
      U.catch
        (fun () -> Lwt_unix.accept bound_socket >>= Lwt.return_ok)
        (fun exn -> 
           Log.err (fun m -> m "Failed while accepting connection");
           Lwt.return_error (`Exn (`Accept, exn)))
      >>>= fun (client_sock, _) ->
      let handler () =
        let main =
          get_id_from_sock client_sock >>= function
          | Error exn ->
              Log.err (fun m -> m "Couldn't get id from sock %a" Fmt.exn exn);
              Lwt.return_unit
          | Ok id ->
              Log.debug (fun m ->
                  m "%a: Got new conn from %a" Fmt.int64 t.node_id Fmt.int64 id);
              conn_from_fd ~switch:client_switch client_sock id `Ephemeral
              >>= fun descr -> handle_new_conn t descr `Incomming
        in
        main
      in
      Lwt.async handler;
      (accept_loop [@tailcall]) ()
    in
    accept_loop ()
  in
  main () >>= fun v ->
  Lwt_switch.turn_off bound_switch >>= fun () ->
  match v with
  | _ when not @@ Lwt_switch.is_on t.switch -> Lwt.return_unit
  | Ok () -> Fmt.failwith "accept loop ended unexpectedly"
  | Error `Closed ->
      Log.debug (fun m -> m "%a: Accept loop closed" Fmt.int64 t.node_id);
      Lwt.return_unit
  | Error (`Exn (`Create, exn)) ->
      Log.err (fun m ->
          m "Accept loop failed during socket creation with %a" Fmt.exn exn);
      Lwt.return_unit
  | Error (`Exn (`Accept, exn)) ->
      Log.err (fun m ->
          m "Accept loop failed while accepting a connection with %a" Fmt.exn
            exn);
      Lwt.return_unit
  | Error _ -> Fmt.failwith "Unknown tag encountered in accept loop"

let create ?(retry_connection_timeout = 10.) ~listen_address ~node_id handler =
  let switch = Lwt_switch.create () in
  let switch_off_prom_fulfill = Lwt.task () in
  let t =
    {
      active_conns = [];
      persist_ids = [];
      retry_connection_timeout;
      handler;
      switch;
      switch_off_prom_fulfill;
      node_id;
    }
  in
  Lwt.async (accept_incomming_loop t listen_address);
  t

let close t =
  Lwt.wakeup (snd t.switch_off_prom_fulfill) ();
  Lwt.pause () >>= fun () -> Lwt_switch.turn_off t.switch

let send ?(semantics = `AtMostOnce) t id msg =
  Log.debug (fun m -> m "%a: Sending to %a" Fmt.int64 t.node_id Fmt.int64 id);
  let err_handler = function
    | Ok () -> Lwt.return_unit
    | Error exn ->
        Log.err (fun m ->
            m "%a: Failed to send message with %a" Fmt.int64 t.node_id Fmt.exn
              exn);
        Lwt.pause ()
  in
  let send () =
    match List.assoc_opt id t.active_conns with
    | None ->
        Lwt.return_error
          (Invalid_argument
             (Fmt.str "Not found %a in active_conns" Fmt.int64 id))
    | Some conn_descr -> Sockets.Outgoing.send conn_descr.out_socket msg
  in
  match semantics with
  | `AtMostOnce -> send () >>= err_handler >>= Lwt.return_ok
  | `AtLeastOnce ->
      let rec loop () =
        send () >>= function
        | Ok v -> Lwt.return v
        | Error Sockets.Closed ->
            Log.debug (fun m ->
                m "%a: Failed to send on closed socket" Fmt.int64 t.node_id);
            Lwt.return ()
        | Error exn ->
            err_handler (Error exn) >>= fun () ->
            Log.debug (fun m -> m "%a: Looping to retry" Fmt.int64 t.node_id);
            (loop [@tailcall]) ()
      in
      loop () >>= fun res -> Lwt.return_ok res
