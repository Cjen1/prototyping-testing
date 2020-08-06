open Lwt.Infix

let main ()= 
  let count = 100 in
  let file = "bench.dat" in
  let size = 512 in

  let fsync = false in
  let o_direct = true in

  let buf = Utils.alloc_buf size in
  let p_randomise =
    let fd = Unix.openfile "/dev/urandom" [Unix.O_RDONLY] 0o666 |> Lwt_unix.of_unix_file_descr in
    Lwt_cstruct.read fd buf >>= fun read ->
    assert(read = size);
    Lwt_unix.close fd
  in p_randomise >>= fun () ->
  let fd = 
    if o_direct then
      Utils.open_direct file false true 0o666
    else 
      Unix.openfile file Unix.[O_WRONLY; O_CREAT; O_TRUNC] 0o666
  in
  Unix.LargeFile.ftruncate fd (size * count |> Int64.of_int);
  Unix.fsync fd;
  let fd = Lwt_unix.of_unix_file_descr fd in
  let rec loop = function
    | 0 -> 
      Lwt.return_unit
    | count ->
      Lwt_cstruct.write fd buf >>= fun written ->
      assert(written = size);
      (
        if fsync then
          Lwt_unix.fsync fd
        else
          Lwt_unix.fdatasync fd
      )
      >>= fun () ->
      loop (count - 1)
  in 
  let run () =
    let start = Unix.gettimeofday () in
    loop count >|= fun () ->
    (Unix.gettimeofday () -. start)
  in
  run () >|= fun duration -> 
  Fmt.pr "Took %.6f seconds per write \n" (duration /. Float.of_int count) 

let () = Lwt_main.run @@ main ()
