let ( let* ) = Lwt.bind

let ( and* ) = Lwt.both

let critical_section mutex ~f =
  try%lwt
    let* () = Logs_lwt.debug (fun m -> m "Entering cs") in
    let* () = Lwt_mutex.lock mutex in
    let* res = f () in
    let () = Lwt_mutex.unlock mutex in
    Lwt.return @@ res
  with e ->
    let () = Lwt_mutex.unlock mutex in
    let* () = Logs_lwt.debug (fun m -> m "Entering cs") in
    raise e

let write_to_wal (oc, fd) line =
  let* res = Lwt_io.write_line oc line in
  let () = Unix.fsync fd in
  Lwt.return res

(* TODO move create_socket out of critical path? *)
let connect uri =
  (* TODO change out from TCP? *)
  let sock = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
  let* () = Lwt_unix.connect sock uri in
  Lwt.return
    ( Lwt_io.of_fd ~mode:Lwt_io.Input sock
    , Lwt_io.of_fd ~mode:Lwt_io.Output sock )

let unix_error_handler (e, f, p) tag =
  let* () =
    Logs_lwt.debug (fun m ->
        m "%s: failed to communicate with %s calling %s with parameter %s" tag
          (Unix.error_message e) f p)
  in
  fst @@ Lwt.task ()

let comm uri msg =
  try%lwt
    let* ic, oc = connect uri in
    let* () = Bytes.to_string msg |> Lwt_io.write_value oc in
    let* bytes = Lwt_io.read_value ic in
    Lwt.return bytes
  with Unix.Unix_error (e, f, p) -> unix_error_handler (e, f, p) "comm"

let send uri msg =
  try%lwt
    let* _, oc = connect uri in
    Lwt_io.write_value oc msg
  with Unix.Unix_error (e, f, p) -> unix_error_handler (e, f, p) "send"

let uri_of_string str =
  match Base.String.split ~on:':' str with
  | [ip; port] ->
      let ip = Unix.inet_addr_of_string ip in
      let port = Base.Int.of_string port in
      Unix.ADDR_INET (ip, port)
  | _ ->
      assert false

let uri_of_string_and_port ip port =
  let ip = Unix.inet_addr_of_string ip in
  Unix.ADDR_INET (ip, port)

module Semaphore = struct
  type t = {m_count: Lwt_mutex.t; m_queue: Lwt_mutex.t; mutable n: int}

  let create n =
    Lwt_main.run
    @@
    let m_count = Lwt_mutex.create () in
    let m_queue = Lwt_mutex.create () in
    let* () = Lwt_mutex.lock m_queue in
    Lwt.return {n; m_count; m_queue}

  let wait t =
    let* () = Lwt_mutex.lock t.m_count in
    t.n <- t.n - 1 ;
    let* () =
      if t.n < 0 then (Lwt_mutex.unlock t.m_count ; Lwt_mutex.lock t.m_queue)
      else Lwt.return_unit
    in
    Lwt.return @@ Lwt_mutex.unlock t.m_count

  let signal t =
    let* () = Lwt_mutex.lock t.m_count in
    t.n <- t.n + 1 ;
    if t.n <= 0 then Lwt_mutex.unlock t.m_queue |> Lwt.return
    else Lwt_mutex.unlock t.m_count |> Lwt.return
end
