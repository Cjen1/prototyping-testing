(* Connection manager for symmetrical connections *)

(* Logging source*)
val src : Logs.src

(* Address type summing over Unix domain sockets or TCP *)
type address = Unix of string | TCP of (string * int)

(* constructs an address for a given string *)
val addr_of_string : string -> (address, [> `Msg of string ]) result

(* pretty printer for addresses *)
val pp_addr : Format.formatter -> address -> unit

(* Connection manager *)
type t

type node_id = int64

(* Handler type for listeners *)
type recv_handler =
  t ->
  node_id ->
  Capnp.MessageSig.ro Capnp.BytesMessage.Message.t ->
  (unit, exn) Lwt_result.t

(* Creates a new connection manager listening on [listen_address] with id: [node_id] *)
val create :
  ?retry_connection_timeout:float ->
  listen_address:address ->
  node_id:node_id ->
  recv_handler ->
  t

(* Closes the connection *)
val close : t -> unit Lwt.t

type conn_kind = [ `Persistant of address | `Ephemeral ]

val add_outgoing : t -> node_id -> address -> conn_kind -> unit Lwt.t

val send :
  ?semantics:[< `AtLeastOnce | `AtMostOnce > `AtMostOnce ] ->
  t ->
  node_id ->
  'a Capnp.BytesMessage.Message.t ->
  (unit, exn) result Lwt.t
