open! Core
open! Core_bench
open Caml
open Caml.Effect
open Caml.Effect.Deep

type _ Effect.t += SendI : int -> unit Effect.t
let sendi m = perform (SendI m)

let runi (kind : [ `Ref | `Fun ]) f =
  match kind with
  | `Ref ->
      let ms = ref [] in
      match_with f ()
        {
          retc = (fun () -> !ms);
          exnc = raise;
          effc =
            (fun (type b) (e : b Effect.t) ->
              match e with
              | SendI m ->
                  Some
                    (fun (k : (b, int list) continuation) ->
                      ms := m :: !ms;
                      continue k ())
              | _ -> None);
        }
  | `Fun ->
      match_with f ()
        {
          retc = (fun () ms -> ms);
          exnc = raise;
          effc =
            (fun (type b) (e : b Effect.t) ->
              match e with
              | SendI m ->
                  Some
                    (fun (k : (b, int list -> int list) continuation) ms ->
                      continue k () (m :: ms))
              | _ -> None);
        }
        []

let rec bench n =
  match n with
  | 0 -> ()
  | _ ->
      sendi n;
      (bench[@tailcall]) (n - 1)

let bench_monad n =
  let bind m f s = match m s with x, s' -> f x s' in
  let return v ms = (v, ms) in
  let send m ms = ((), m :: ms) in
  let ( let* ) = bind in
  let rec loop n =
    match n with
    | 0 -> return ()
    | _ ->
        let* () = send n in
        (loop[@tailcall]) (n - 1)
  in
  loop n []

let bench_ref n =
  let ms = ref [] in
  let rec loop n =
    match n with
    | 0 -> ()
    | _ ->
        ms := n :: !ms;
        (loop[@tailcall]) (n - 1)
  in
  loop n

let () =
  let n = 10 in
  [%message "Starting test" (n : int)] |> Sexp.to_string_hum |> print_endline;
  let eff () = bench n in
  Command_unix.run
    (Bench.make_command
       [
         Bench.Test.create ~name:"EffFun" (fun () -> runi `Fun eff);
         Bench.Test.create ~name:"EffRef" (fun () -> runi `Ref eff);
         Bench.Test.create ~name:"Monads" (fun () -> bench_monad n);
         Bench.Test.create ~name:"SimRef" (fun () -> bench_ref n);
       ])
