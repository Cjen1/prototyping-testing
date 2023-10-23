open Eio.Std

let run n =
  let ( let* ) k f = k f in
  let* env = Eio_main.run in
  traceln "Running %s" env#backend_id;
  let* sw = Switch.run in
  let rec daemon () =
    Eio.Time.sleep env#clock 1. ;
    traceln "still running" ;
    daemon ()
  in
  traceln "start" ;
  Fiber.fork_daemon ~sw daemon ;
  let* stdout = Eio.Buf_write.with_flow ~initial_size:8192 env#stdout in
  let write_size = ref 0 in
  let write buf = Eio.Buf_write.uint8 buf 1 ; incr write_size in
  let rec aux n =
    if n = 0 then ()
    else (
      write stdout ;
      aux (n - 1) )
  in
  aux n ;
  Eio.Buf_write.flush stdout ;
  Eio.traceln "Writing %d bytes" !write_size ;
  traceln "done"

open Cmdliner

let () =
  let n_t =
    Arg.(required & pos 0 (some int) None (info ~docv:"N" ~doc:"" []))
  in
  Cmd.eval (Cmd.v (Cmd.info "eiopipe") Term.(const run $ n_t)) |> exit
