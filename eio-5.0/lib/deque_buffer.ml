open! Eio

type bigstring = Bigstringaf.t

exception Dequeue_empty

module Deque(T:sig type t val sentinel : t end) : sig
  type elem = T.t

  type t

  val create : int -> t
  (* [t = create n] creates a new deque with initial capacity [n].
     [to_list t = []] *)

  val is_empty : t -> bool
  (* [is_empty t = (to_list t = []) *)

  val enqueue : elem -> t -> unit
  (* [enqueue elem t]
     [to_list t'] = to_list t @ [elem] *)

  val dequeue_exn : t -> elem
  (* [dequeue_exn t = List.hd (to_list t)]
     [to_list t' = List.tl (to_list t)] *)

  val enqueue_front : elem -> t -> unit
  (* [enqueue_front elem t]
     to_list t' = elem :: to_list t *)

  val to_list : t -> elem list
end = struct
  type elem = T.t

  type t =
    { mutable elements : elem array
    ; mutable front    : int
    ; mutable back     : int }

  let sentinel = T.sentinel

  let create size =
    { elements = Array.make size sentinel; front = 0; back = 0 }

  let is_empty t =
    t.front = t.back

  let ensure_space t =
    if t.back = Array.length t.elements - 1 then begin
      let len = t.back - t.front in
      if t.front > 0 then begin
        (* Shift everything to the front of the array and then clear out
         * dangling pointers to elements from their previous locations. *)
        Array.blit t.elements t.front t.elements 0 len;
        Array.fill t.elements len t.front sentinel
      end else begin
        let old  = t.elements in
        let new_ = Array.(make (2 * length old) sentinel) in
        Array.blit old t.front new_ 0 len;
        t.elements <- new_
      end;
      t.front <- 0;
      t.back <- len
    end

  let enqueue e t =
    ensure_space t;
    t.elements.(t.back) <- e;
    t.back <- t.back + 1

  let dequeue_exn t =
    if is_empty t then
      raise Dequeue_empty
    else
      let result = Array.unsafe_get t.elements t.front in
      Array.unsafe_set t.elements t.front sentinel;
      t.front <- t.front + 1;
      result

  let enqueue_front e t =
    (* This is in general not true for Deque data structures, but the usage
     * below ensures that there is always space to push an element back on the
     * front. An [enqueue_front] is always preceded by a [dequeue], with no
     * intervening operations. *)
    assert (t.front > 0);
    t.front <- t.front - 1;
    t.elements.(t.front) <- e

  let to_list t =
    let result = ref [] in
    for i = t.back - 1 downto t.front do
      result := t.elements.(i) :: !result
    done;
    !result
end

module Buffers = Deque(struct
  type t = Cstruct.t
  let sentinel =
    let deadbeef = "\222\173\190\239" in
    Cstruct.of_string deadbeef
end)

