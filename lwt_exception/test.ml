(*let ( let* ) = Lwt.bind

exception TestExn of int

let main () = 
  let x = [1;2;3;4;] in
  let px = List.map (fun x -> 
      let* x = Lwt.return (x + 1) in
      if x == 2 || x == 5
      then raise @@ TestExn x
      else let* () = Lwt.pause () in
        Lwt.return x
    ) x
  in
  Lwt.choose px

let () = 
  print_int @@ Lwt_main.run @@ main ()
*)

let () = 
  let x = [1;2;3;4;5] in
  let xs = Base.List.fold_left x ~init:[] ~f:(fun xs x -> x :: xs) in
  let xs' = Base.List.fold_right x ~init:[] ~f:(fun x xs -> x :: xs) in
  Printf.printf "x = xs: %b\n" (xs = x);
  Printf.printf "x = xs': %b\n" (xs' = x)
