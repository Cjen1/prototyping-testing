let main lwt = 
  let count = 100 in
  let file = "bench.dat" in
  let size = 512 in

  let fsync = false in
  let o_direct = false in

  let buf = Bytes.create size in
  let () = 
    let fd = Unix.openfile "/dev/urandom" [Unix.O_RDONLY] 0o666 in
    assert (Unix.read fd buf 0 size = size);
    Unix.close fd
  in 
  let fd = 
    if o_direct then
      Utils.open_direct file false true 0o666
    else 
      Unix.openfile file Unix.[O_WRONLY; O_CREAT; O_TRUNC] 0o666
  in
  Unix.LargeFile.ftruncate fd (size * count |> Int64.of_int);
  Unix.fsync fd;
  let duration = match lwt with
    | false ->
      let start = Unix.gettimeofday () in
      for _ = 0 to count do
        assert (Unix.write fd buf 0 size = size);
        if fsync then
          Unix.fsync fd
        else
          Utils.fdatasync fd
      done;
      (Unix.gettimeofday () -. start)
    | true ->
      let lfd = Lwt_unix.of_unix_file_descr fd in
      let open Lwt.Infix in
      let rec loop = function
        | 0 -> 
          Lwt.return_unit
        | count ->
          Lwt_unix.write lfd buf 0 size >>= fun written ->
          assert(written = size);
          (*
          if fsync then
            Unix.fsync fd
          else
            Utils.fdatasync fd
          ;*)
          Lwt_unix.fdatasync lfd >>= fun () ->
          loop (count - 1)
      in 
      let run () =
        let start = Unix.gettimeofday () in
        loop count >|= fun () ->
        (Unix.gettimeofday () -. start)
      in
      Lwt_main.run (run ())
  in 
  Fmt.pr "Took %.2f seconds for %s\n" duration (if lwt then "lwt" else "straight") 

let () = main false; main true
