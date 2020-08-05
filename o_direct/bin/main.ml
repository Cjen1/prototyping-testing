open Lwt.Infix
let (>>>=) = Lwt_result.bind

let src = Logs.Src.create "log"

module L = (val Logs.src_log src : Logs.LOG)

module C = Cstruct
module U = Utils

let main ?(count=1000) ?(file="bench.data") ?(size=1) () =
  print_endline "Starting";
  let buf = Io_page.get_buf ~n:1 () in
  let buf = C.sub buf 0 size in
  print_endline "Allocated buffer";
  let () =
    let udev_buf = Bytes.create size in
    let fd = Unix.openfile "/dev/urandom" [Unix.O_RDONLY] 0 in 
    assert(Unix.read fd udev_buf 0 size = size);
    Unix.close fd;
    C.blit_from_bytes udev_buf 0 buf 0 size;
  in 
  print_endline "Set up buffer";
  let fd = U.open_direct file false true 0o666 |> Lwt_unix.of_unix_file_descr ~blocking:true ~set_flags:false in 
  U.ftruncate fd Int64.(of_int @@ size * count) >>= fun () ->
  Lwt_unix.fsync fd >>= fun () ->

  let rec loop = function
    | 0 -> Lwt.return_unit 
    | count ->
      let write c = 
        Lwt_cstruct.write fd c
      in 
      Lwt_cstruct.complete write buf >>= fun () ->
      Lwt_unix.fdatasync fd >>= fun () ->
      loop (count - 1)
  in
  print_endline "Starting Loop";
  let start = Unix.gettimeofday () in
  loop count >|= fun () ->
  Fmt.pr "Completed in %f seconds/n" (Unix.gettimeofday () -. start)

let () = Lwt_main.run @@ main ~count:1 ()
  

(*
type partial_sector = {
  mutable buffer : Cstruct.t
; mutable underlying : Cstruct.t
; mutable buffer_start : int64
}

let create_partial sector_size size = 
  L.debug (fun m -> m "Creating %d %d" sector_size size);
  let underlying = U.alloc_buf size in
  let underlying = C.sub underlying 0 sector_size in
  assert (C.check_alignment underlying sector_size);
  let buffer = C.sub underlying 8 (C.len underlying - 8) in
  { buffer
  ; underlying
  ; buffer_start=Int64.zero
  }

let pp_sector ppf t =
  Format.fprintf ppf "[%s,%s,%a]" (C.debug t.buffer) (C.debug t.underlying) Fmt.int64 t.buffer_start

let access : partial_sector -> int -> (Cstruct.t -> unit) -> (partial_sector, [`OOM]) Result.t = fun t size f ->
  L.debug (fun m -> m "accessing %a" pp_sector t);
  if C.check_bounds t.buffer size then
    let buf,buffer = C.split t.buffer size in
    let buffer_start = Int64.(add t.buffer_start @@ of_int size) in 
    f buf;
    t.buffer <- buffer;
    t.buffer_start <- buffer_start;
    L.debug (fun m -> m "updated to %a" pp_sector t);
    Ok t
  else
    (
      L.debug (fun m -> m "OOM");
      Error `OOM
    )

type file_wrapper = {
  path : string
; dev : B.t
; sector_size : int
; buffer_size : int
; mutable current_sector_start : int64 (* Monotonically increases and can only be written to once*) 
; mutable current_sector : partial_sector option
}

let pp_file ppf t =
  Format.fprintf ppf "[%d,%d,%a,%a]" t.sector_size t.buffer_size Fmt.int64 t.current_sector_start Fmt.(option pp_sector) t.current_sector

let write_advance file = 
  match file.current_sector with
  | Some ps ->
    L.debug (fun m -> m "%a" pp_file file);
    let sectors_written = C.len ps.underlying / file.sector_size in
    let underlying, sector_start = ps.underlying, file.current_sector_start in
    file.current_sector_start <- Int64.(add file.current_sector_start @@ of_int sectors_written);  
    file.current_sector <- None; 
    C.LE.set_uint64 ps.underlying 0 ps.buffer_start;
    L.debug (fun m -> m "Writing...   ");
    let fd : Lwt_unix.file_descr = assert false in
    Lwt_bytes.write fd underlying.C.buffer underlying.C.off underlying.C.len >>= fun _i ->
    B.write file.dev sector_start [underlying]
  | None ->
    Lwt.return_ok ()

let access_file file size f =
  let ps = 
    match file.current_sector with
    | None ->
      create_partial file.sector_size size
    | Some ps -> 
      ps
  in
  match access ps size f with
  | Ok ps -> 
    file.current_sector <- Some ps
  | Error `OOM ->
    L.debug (fun m -> m "OOM");
    Lwt.on_failure (write_advance file) !Lwt.async_exception_hook (* Write sector to disk asynchronously *)

let connect ~buffered ~sync ?buffer_size path =
  let config = B.Config.create ~buffered ~sync path in
  B.of_config config >>= fun dev ->
  B.get_info dev >>= fun info ->
  let buffer_size = match buffer_size with
    | Some size -> size
    | None -> info.sector_size
  in 
  let sector_size = info.sector_size in
  let current_sector_start = Int64.zero in
  let current_sector = None in
  Lwt.return
    {path; dev; sector_size; current_sector_start; current_sector; buffer_size}

let flush file =
  write_advance file >>>= fun () ->
  B.flush file.dev

let write_test ?(count=1000) ?(filename = "bench.data") ?(buffered=false) ~size () =
  L.debug (fun m -> m "Connecting/n");
  connect ~buffered ~sync:(Some `ToDrive) filename >>= fun file ->
  (*
  B.resize file.dev (Int64.mul 1024L 1024L) >>= fun _ ->
     *)
  B.resize file.dev Int64.(mul 1024L 1024L) >>= fun _ ->
  let msg = "testing testing 123\n" in
  let alteration c =
    for i = 0 to (size - 1) do
      C.set_char c i (msg.[i mod (String.length msg)])
    done;
    L.debug (fun m -> m "%s" (C.debug c)) 
  in
  let rec loop = function
    | n when n = 0 ->
      Lwt.return_ok ()
    | n ->
      access_file file size alteration;
      flush file >>>= fun () -> loop n
  in 
  L.debug (fun m -> m "Starting test");
  let start = Unix.gettimeofday () in
  loop count >>= fun _ ->
  Lwt.return ((Unix.gettimeofday () -. start) /. (Int.to_float count))

let reporter =
  let report src level ~over k msgf =
    let open Core in
    let k _ = over () ; k () in
    let src = Logs.Src.name src in
    msgf
    @@ fun ?header ?tags:_ fmt ->
    Fmt.kpf k Fmt.stdout
      ("[%a] %a %a @[" ^^ fmt ^^ "@]@.")
      Time.pp (Time.now ())
      Fmt.(styled `Magenta string)
      (Printf.sprintf "%14s" src)
      Logs_fmt.pp_header (level, header)
  in
  {Logs.report}

let () =
  Logs.set_reporter reporter;
  Logs.(set_level (Some Info)) ;
  let latency = Lwt_main.run (write_test ~buffered:false ~size:8 ()) in
  L.debug (fun m -> m "Latency = %.5f" latency)

(*
   How to do a WAL:
    - Take op and serialise it into current sector buffer
    - If overrun sector, continue to serialise onto next sector buf (This may become difficult without memory copying...)
    - Write sector(s) to disk, dump buffers for any full sectors (including fsync'ing etc)
    - Potentially batch until sector buffer is full then write
*)
   *)