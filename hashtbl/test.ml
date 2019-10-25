open Base

let () =
  let tbl = Hashtbl.create (module Int) in
  Hashtbl.set tbl ~key:1 ~data:1;
  Hashtbl.change tbl 1 ~f:(fun o -> match o with
      | Some(i) -> Some(i+1)
      | None -> Some(5)
    );
  Stdlib.print_endline @@ Printf.sprintf "i = %d" (Hashtbl.find_exn tbl 1);
  Hashtbl.change tbl 1 ~f:(fun o -> match o with
      | Some(_) -> None
      | None -> Some(5)
    );
  Stdlib.print_endline @@ Printf.sprintf "i = %d" (Hashtbl.find_exn tbl 1)
