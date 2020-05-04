module Api = Echo_api.MakeRPC(Capnp_rpc_lwt)

open Lwt.Infix
open Capnp_rpc_lwt

module Callback = struct
  module Callback = Api.Client.Callback

  type t

  let log t msg =
    let open Callback.Log in
    let request, params = Capability.Request.create Params.init_pointer in
    Params.msg_set params msg;
    Capability.call_for_unit t method_id request

  let local = 
    Callback.local
end


(*-- Server ----------------------------------------*)
let local =
  let module Echo = Api.Service.Echo in

  let (>>!=) = Lwt_result.bind in 
  let notify callback ~msg =
    let rec loop = function
      | 0 ->
        Lwt.return @@ Ok (Service.Response.create_empty ())
      | i ->
        Callback.log callback msg >>!= fun () ->
        Lwt_unix.sleep 1.0 >>= fun () ->
        loop (i - 1)
    in
    loop 3 
  in

  Echo.local @@ object
    inherit Echo.service

    method ping_impl params release_param_caps =
      let open Echo.Ping in
      let msg = Params.msg_get params in
      release_param_caps ();
      let response, results = Service.Response.create Results.init_pointer in
      Results.reply_set results ("echo:" ^ msg);
      Service.return response

    method heartbeat_impl params release_param_caps =
      let open Echo.Heartbeat in
      let msg = Params.msg_get params in
      let callback = Params.callback_get params in
      release_param_caps ();
      match callback with
      | None -> Service.fail "No callback parameter!"
      | Some callback ->
        Service.return_lwt @@ fun () ->
        Capability.with_ref callback (notify ~msg)
  end

(*-- Client ----------------------------------------*)
module Echo = Api.Client.Echo

let ping t msg =
  let open Echo.Ping in
  let request, params = Capability.Request.create Params.init_pointer in
  Params.msg_set params msg;
  Capability.call_for_value_exn t method_id request >|= Results.reply_get

let heartbeat t msg callback =
  let open Echo.Heartbeat in
  let request, params = Capability.Request.create Params.init_pointer in
  Params.msg_set params msg;
  Params.callback_set params (Some callback);
  Capability.call_for_unit_exn t method_id request



