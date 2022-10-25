open Effect
open Effect.Deep

type msg =
  | A of string
  | B of string
  | C of string

let msg_to_str msg = match msg with
| A s -> Printf.sprintf "A(%s)" s
| B s -> Printf.sprintf "B(%s)" s
| C s -> Printf.sprintf "C(%s)" s

type _ Effect.t += Send : msg -> unit Effect.t
type _ Effect.t += Fail : string -> unit Effect.t

let send m = perform (Send m)
let fail m = perform (Fail m)

let main () =
  send (A "foo");
  send (B "bar");
  fail "exn";
  send (C "foobar")

exception Discontinued

let run f =
  match_with f () {
    retc = (fun msgs -> msgs);
    exnc = (function 
      | Discontinued -> fun msgs -> msgs
      | e -> raise e );
    effc = fun (type b) (e : b Effect.t) ->
      match e with
      | Send m -> Some ( fun (k : (unit, msg list -> msg list) continuation) ->
          fun x -> continue k x ())
      | Fail m -> Some ( fun (k : (unit, msg list -> msg list) continuation) ->
          (Printf.printf "Failed with %s\n" m; fun x -> discontinue k Discontinued ())
      )
      | _ -> None
  } []

let () = 
  let msgs = run main in
  List.iter (fun m -> Printf.printf "Received: \"%s\"\n" @@ msg_to_str m) (List.rev msgs)
