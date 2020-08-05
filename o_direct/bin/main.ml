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
  
