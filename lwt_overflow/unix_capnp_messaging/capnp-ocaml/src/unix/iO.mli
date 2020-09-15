(******************************************************************************
 * capnp-ocaml
 *
 * Copyright (c) 2013-2014, Paul Pelzl
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimer.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 ******************************************************************************)

open Capnp

exception Unsupported_message_frame

module WriteContext : sig
  type 'a t

  (** [create ~write ~compression descr] creates a new context for writing
      data to the specified descriptor.  [compression] specifies the compression
      format, if any.

      The semantics of the [write] function shall mimic that of [Unix.single_write],
      attempting to write a substring from the [buf] and returning the number
      of bytes actually written. *)
  val create : write:('a -> buf:string -> pos:int -> len:int -> int) ->
    compression:Codecs.compression_t -> 'a -> 'a t

  (** [enqueue_message context message] places the [message] in the outgoing
      queue of the write [context].  No data will be written to the underlying
      descriptor. *)
  val enqueue_message : 'a t -> 'cap Message.BytesMessage.Message.t -> unit

  (** [bytes_remaining context] obtains the number of unwritten bytes currently
      stored in the write [context]. *)
  val bytes_remaining : 'a t -> int

  (** [write context] attempts to write some of the queued data to the
      underlying descriptor associated with the write [context].

      @return the number of bytes actually written *)
  val write : 'a t -> int

  (** [write_message context message] places the [message] in the outgoing
      queue of the write [context], then invokes [write] in a loop until all
      queued data has been written. *)
  val write_message : 'a t -> 'cap Message.BytesMessage.Message.t -> unit
end


module ReadContext : sig
  type 'a t

  (** [create ~read ~compression descr] creates a new context for reading data
      from the specified descriptor using the specified [compression] method.

      The semantics of the [read] function shall mimic that of [Unix.read],
      attempting to read into a substring of the [buf] and returning the number
      of bytes actually read.  A read of length 0 indicates end-of-file. *)
  val create : read:('a -> buf:Bytes.t -> pos:int -> len:int -> int) ->
    compression:Codecs.compression_t -> 'a -> 'a t

  (** [dequeue_message context] attempts to remove a complete message from the
      incoming queue of the read [context].  No data will be read from the
      underlying descriptor.

      @return None if there is insufficient data stored in the read context
      to decode a complete message

      @raise Unsupported_message_frame if the frame header describes a segment
      count or segment size that is too large for the implementation *)
  val dequeue_message : 'a t -> Message.rw Message.BytesMessage.Message.t option

  (** [bytes_available context] obtains the number of bytes already read which
      are currently stored in the read [context]. *)
  val bytes_available : 'a t -> int

  (** [read context] attempts to read some data from the underlying descriptor,
      storing it in the read [context].

      @return the number of bytes actually read *)
  val read : 'a t -> int

  (** [read_message context] attempts to remove a complete message from the
      incoming queue of the read [context].  [read] may be invoked repeatedly
      to provide enough buffered data to decode a complete message.

      @return None if end-of-file was reached before a complete message could
      be read

      @raise Unsupported_message_frame if the frame header describes a segment
      count or segment size that is too large for the implementation *)
  val read_message : 'a t -> Message.rw Message.BytesMessage.Message.t option

end


(** [create_write_context_for_fd ~compression fd] creates a context for writing
    messages to the given file descriptor using the specified [compression] format.
    If [restart] is set to [true] (default), then writes failing with error
    code Unix.EINTR will be automatically restarted. *)
val create_write_context_for_fd : ?restart:bool -> compression:Codecs.compression_t ->
  Unix.file_descr -> Unix.file_descr WriteContext.t


(** [create_write_context_for_channel ~compression chan] creates a context for
    writing messages to the given buffered output channel using the specified
    [compression] format. *)
val create_write_context_for_channel : compression:Codecs.compression_t ->
  out_channel -> out_channel WriteContext.t


(** [create_read_context_for_fd ~compression fd] creates a context for reading
    messages from the given file descriptor using the specified [compression]
    method. If [restart] is set to [true] (default), then writes failing with
    error code Unix.EINTR will be automatically restarted. *)
val create_read_context_for_fd : ?restart:bool -> compression:Codecs.compression_t ->
  Unix.file_descr -> Unix.file_descr ReadContext.t


(** [create_read_context_for_channel ~compression chan] creates a context for
    reading messages from the given input channel using the specified
    [compression] method. *)
val create_read_context_for_channel : compression:Codecs.compression_t ->
  in_channel -> in_channel ReadContext.t


(** [write_message_to_fd ~compression message fd] writes the specified [message] to
    the given file descriptor, using the specified [compression] method.
    If [restart] is set to [true] (default), then writes failing with error
    code Unix.EINTR will be automatically restarted.

    EAGAIN/EWOULDBLOCK are handled automatically.

    @raise Unix.Unix_error if a write fails *)
val write_message_to_fd : ?restart:bool -> compression:Codecs.compression_t ->
  'cap Message.BytesMessage.Message.t -> Unix.file_descr -> unit


(** [write_message_to_channel ~compression message chan] writes the specified
    [message] to the given buffered I/O channel, using the specified [compression]
    method. *)
val write_message_to_channel : compression:Codecs.compression_t ->
  'cap Message.BytesMessage.Message.t -> out_channel -> unit


(** [write_message_to_file ~compression message filename] writes the specified
    [message] to a file with the given [filename], using the requested
    [compression] method.  The optional [perm] specifies the file creation
    mode, in case a new file must be created. *)
val write_message_to_file : ?perm:int -> compression:Codecs.compression_t ->
  'cap Message.BytesMessage.Message.t -> string -> unit


(** [read_single_message_from_fd ~compression fd] attempts to read a single
    message from the specified file descriptor, using the given [compression]
    method.  If [restart] is set to [true] (default), then writes failing
    with error code Unix.EINTR will be automatically restarted.

    EAGAIN/EWOULDBLOCK are handled automatically.

    This function is appropriate to use only when 0 or 1 messages are expected
    to be available from the descriptor; if additional messages are available,
    some message data could be lost.  Use [create_XXX_read_context_for_fd] to
    correctly handle a stream of messages.

    @return Some message, or None if end-of-file was reached before a message
    could be decoded.

    @raise Unix.Unix_error if a read fails

    @raise Unsupported_message_frame if the frame header describes a segment
    count or segment size that is too large for the implementation *)
val read_single_message_from_fd : ?restart:bool -> compression:Codecs.compression_t ->
  Unix.file_descr -> Message.rw Message.BytesMessage.Message.t option


(** [read_single_message_from_channel ~compression chan] attempts to read a single
    message from the specified input channel, using the given [compression]
    method.

    This function is appropriate to use only when 0 or 1 messages are expected
    to be available from the channel; if additional messages are available,
    some message data could be lost.  Use [create_XXX_read_context_for_channel]
    to correctly handle a stream of messages.

    @return Some message, or None if end-of-file was reached before a message
    could be decoded.

    @raise Unsupported_message_frame if the frame header describes a segment
    count or segment size that is too large for the implementation *)
val read_single_message_from_channel : compression:Codecs.compression_t ->
  in_channel -> Message.rw Message.BytesMessage.Message.t option


(** [read_message_from_file ~compression filename] attempts to read a
    message from the file with the given [filename], using the requested
    [compression] method.

    @return Some message, or None if the file does not contain a complete
    message frame. *)
val read_message_from_file : compression:Codecs.compression_t -> string ->
  Message.rw Message.BytesMessage.Message.t option


