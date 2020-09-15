let test n =
 let start, f = Lwt.task () in
 let open Lwt.Infix in
 let rec loop prev = function
 | 0 -> prev
 | n ->
   let curr = prev >>= fun i -> i + 1 |> Lwt.return in
   loop curr (n-1)
 in
 let end_p = loop start n in
 Lwt.wakeup f 0;
 end_p

let () = 
  Printexc.record_backtrace true;
  let n = 100000 in
  let res = Lwt_main.run @@ test n in
  assert (n = res)
