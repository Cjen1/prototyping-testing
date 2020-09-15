open Core
open Async

let test n =
 let start = Async.Ivar.create () in
 let rec loop prev = function
 | 0 -> prev
 | n ->
   let curr = prev >>= fun i -> i + 1 |> return in
   loop curr (n-1)
 in
 let end_p = loop (Ivar.read start) n in
 Ivar.fill start 0;
 end_p

(* I think this works because it explicitly schedules the deferred rather than deepening the stack *)

let () = 
  Printexc.record_backtrace true;
  let n = 10000000 in
  upon (test n) (fun res -> 
      print_endline "Successful";
      assert (n = res);
      shutdown 0
    )

let () = never_returns (Scheduler.go ())
