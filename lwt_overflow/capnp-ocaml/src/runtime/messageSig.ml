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

module Uint32 = Stdint.Uint32
module Uint64 = Stdint.Uint64

type ro
type rw

module type SEGMENT = sig
  (** [storage_t] is the type of the underlying storage associated with
      this segment (e.g. "bytes"). *)
  type storage_t

  (** ['cap t] is the type of a message segment.  The ['cap] annotation is
      type [ro] for read-only segments, and type [rw] for read/write
      segments. *)
  type -'cap t

  (** [alloc size] allocates a new zero-filled message segment of at least
      [size] bytes, raising an exception if storage cannot be allocated. *)
  val alloc : int -> rw t

  (** [release s] immediately releases the storage associated with message
      segment [s], potentially making the storage available for future
      allocations.  After releasing a storage segment, the behavior of
      the accessor functions is undefined. *)
  val release : 'cap t -> unit

  (** [length s] determines the length of this message segment. *)
  val length : 'cap t -> int

  (** [readonly s] obtains a view of segment [s] which is read-only qualified. *)
  val readonly : 'cap t -> ro t

  (** [of_storage storage] constructs a read/write segment which uses the given
      [storage] for the underlying storage media. *)
  val of_storage : storage_t -> rw t

  (** [to_storage s] retrieves the underlying storage media associated with
      segment [s]. *)
  val to_storage : 'cap t -> storage_t

  (** [get_uintXX s ofs] reads an unsigned integer of the specified width,
      starting at byte offset [ofs] within message segment [s]. *)

  val get_uint8  : 'cap t -> int -> int
  val get_uint16 : 'cap t -> int -> int
  val get_uint32 : 'cap t -> int -> Uint32.t
  val get_uint64 : 'cap t -> int -> Uint64.t

  (** [get_intXX s ofs] reads a signed integer of the specified width,
      starting at byte offset [ofs] within message segment [s]. *)

  val get_int8   : 'cap t -> int -> int
  val get_int16  : 'cap t -> int -> int
  val get_int32  : 'cap t -> int -> Int32.t
  val get_int64  : 'cap t -> int -> Int64.t

  (** [set_uintXX s ofs val] writes the value of the width-restricted
      unsigned integer [val] into read/write-qualified message segment [s],
      starting at byte offset [ofs]. *)

  val set_uint8  : rw t -> int -> int -> unit
  val set_uint16 : rw t -> int -> int -> unit
  val set_uint32 : rw t -> int -> Uint32.t -> unit
  val set_uint64 : rw t -> int -> Uint64.t -> unit

  (** [set_intXX s ofs val] writes the value of the width-restricted
      signed integer [val] into read/write-qualified message segment [s],
      starting at byte offset [ofs]. *)

  val set_int8  : rw t -> int -> int -> unit
  val set_int16 : rw t -> int -> int -> unit
  val set_int32 : rw t -> int -> Int32.t -> unit
  val set_int64 : rw t -> int -> Int64.t -> unit

  (** [blit ~src ~src_pos ~dst ~dst_pos ~len] transfers [len] bytes
      from position [dst_pos] in [dst] to position [src_pos] in [pos].
      The blit operation shall work correctly even for the case of
      overlapping buffers. *)
  val blit : src:('cap t) -> src_pos:int ->
    dst:(rw t) -> dst_pos:int -> len:int -> unit

  (** As [blit], but the destination is a [bytes] buffer. *)
  val blit_to_bytes : src:('cap t) -> src_pos:int ->
    dst:Bytes.t -> dst_pos:int -> len:int -> unit

  (** As [blit], but the source is a [string] buffer. *)
  val blit_from_string : src:string -> src_pos:int ->
    dst:(rw t) -> dst_pos:int -> len:int -> unit

  (** [zero_out segment ~pos ~len] sets [len] bytes of the segment
      to zero, beginning at byte offset [pos]. *)
  val zero_out : rw t -> pos:int -> len:int -> unit
end

(** An RPC message can have an array of attached capabilities.
    To avoid making the message layer depend on the RPC layer, we define this
    as an open type, so that an RPC system can use whatever system it likes for
    this. *)
type attachments = ..

(** This is the default attachment handler. *)
type attachments += No_attachments

module type MESSAGE = sig
  (** [storage_t] is the type of the underlying storage associated with
      this segment (e.g. "bytes"). *)
  type storage_t

  type storage_descr_t = {
    (** Storage for one of the message segments *)
    segment : storage_t;

    (** Number of bytes actually consumed in this segment *)
    bytes_consumed : int;
  }

  type -'cap segment_t

  (** ['cap t] is the type of a message.  The ['cap] annotation is type [ro]
      for read-only segments, and type [rw] for read/write segments. *)
  type -'cap t

  (** [create size] allocates a new zero-filled single-segment message of at
      least [size] bytes, raising an exception if storage cannot be allocated. *)
  val create : int -> rw t

  (** [release m] immediately releases the storage for all segments of message
      [m], potentially making the storage available for future allocations.
      After releasing a storage segment, the behavior of the accessor functions
      is undefined. *)
  val release : 'cap t -> unit

  (** [num_segments m] obtains the number of segments associated with message [m]. *)
  val num_segments : 'cap t -> int

  (** [total_size m] gets the total size of the message, in bytes, across all
      segments. *)
  val total_size : 'cap t -> int

  (** [total_alloc_size m] gets total size of the underlying storage for the
      message, in bytes, across all segments.  (This is at least as large
      as the value returned by [total_size]. *)
  val total_alloc_size : 'cap t -> int

  (** [get_segment m i] gets zero-indexed segment [i] associated with message [m].
      @raise Invalid_argument if the index is out of bounds. *)
  val get_segment : 'cap t -> int -> 'cap segment_t

  (** [readonly m] obtains a view of message [m] which is read-only qualified. *)
  val readonly :'cap t -> ro t

  (** [of_storage chunks] constructs a read/write message which uses the list of
      storage [chunks] as the underlying storage media for the message segments. *)
  val of_storage : storage_t list -> rw t

  (** [to_storage m] retrieves a list of the storage elements associated with
      the message segments. *)
  val to_storage : 'cap t -> storage_descr_t list

  (** [with_message m ~f] first evaluates [f m], then invokes [release m], then
      returns the result of the application of [f].  If [f m] raises an exception,
      the exception will be propagated after a call to [release]. *)
  val with_message : 'cap t -> f:('cap t -> 'a) -> 'a

  (** [with_attachments attachments m] is a message sharing the same storage as [m],
      but with the given attachments. If [m] is mutable, it should not be used after
      calling this. Effectively, [attachments] is a constructor argument, but it isn't
      known until slightly after the message is constructed. *)
  val with_attachments : attachments -> 'cap t -> 'cap t

  (** [get_attachments m] returns the handler previously set in [with_attachments],
      or [No_attachments] if no attachments were given. *)
  val get_attachments : 'cap t -> attachments
end

module type SLICE = sig
  type -'cap segment_t
  type -'cap message_t

  (** Type [t] represents a contiguous range of bytes associated with a single
      segment of a message. *)
  type 'cap t = {
    msg        : 'cap message_t;  (** Identifies the message of interest *)
    segment    : 'cap segment_t;  (** Segment within message housing these bytes *)
    segment_id : int;             (** Index of the segment *)
    start      : int;             (** Starting byte of the slice *)
    len        : int;             (** Length of the slice, in bytes *)
  }

  (** [alloc m size] reserves [size] bytes of space within message [m].  This
      may result in extending the message with an additional segment; if
      storage cannot be allocated for a new segment, an exception is raised.
      Note that the allocated slices always begin on an eight-byte boundary. *)
  val alloc : rw message_t -> int -> rw t

  (** [alloc_in_segment m seg_id size] attempts to reserve [size] bytes of space
      within segment [seg_id] of message [m].  Allocation will fail if the
      segment is full. *)
  val alloc_in_segment : rw message_t -> int -> int -> rw t option

  (** [get_segment slice] gets the message segment associated with the [slice]. *)
  val get_segment : 'cap t -> 'cap segment_t

  (** [get_end slice] computes [slice.start] + [slice.len]. *)
  val get_end : 'cap t -> int

  (** [readonly s] obtains a view of slice [s] which is read-only qualified. *)
  val readonly : 'cap t -> ro t

  (** [get_uintXX s ofs] reads an unsigned integer of the specified width,
      starting at byte offset [ofs] within the [slice]. *)

  val get_uint8  : 'cap t -> int -> int
  val get_uint16 : 'cap t -> int -> int
  val get_uint32 : 'cap t -> int -> Uint32.t
  val get_uint64 : 'cap t -> int -> Uint64.t

  (** [get_intXX s ofs] reads a signed integer of the specified width,
      starting at byte offset [ofs] within the [slice]. *)

  val get_int8   : 'cap t -> int -> int
  val get_int16  : 'cap t -> int -> int
  val get_int32  : 'cap t -> int -> Int32.t
  val get_int64  : 'cap t -> int -> Int64.t

  (** [set_uintXX s ofs val] writes the value of the width-restricted
      unsigned integer [val] into the read/write-qualified [slice],
      starting at byte offset [ofs]. *)

  val set_uint8  : rw t -> int -> int -> unit
  val set_uint16 : rw t -> int -> int -> unit
  val set_uint32 : rw t -> int -> Uint32.t -> unit
  val set_uint64 : rw t -> int -> Uint64.t -> unit

  (** [set_intXX s ofs val] writes the value of the width-restricted
      signed integer [val] into the read/write-qualified [slice],
      starting at byte offset [ofs]. *)

  val set_int8   : rw t -> int -> int -> unit
  val set_int16  : rw t -> int -> int -> unit
  val set_int32  : rw t -> int -> Int32.t -> unit
  val set_int64  : rw t -> int -> Int64.t -> unit

  (** [blit ~src ~src_pos ~dst ~dst_pos ~len] copies [len] bytes from the
      source slice (beginning at [src_pos]) to the destination slice
      (beginning at [dst_pos]). *)
  val blit : src:('cap t) -> src_pos:int ->
    dst:(rw t) -> dst_pos:int -> len:int -> unit

  (** As [blit], but the destination is a [bytes] buffer. *)
  val blit_to_bytes : src:('cap t) -> src_pos:int ->
    dst:Bytes.t -> dst_pos:int -> len:int -> unit

  (** As [blit], but the source is a [string] buffer. *)
  val blit_from_string : src:string -> src_pos:int ->
    dst:(rw t) -> dst_pos:int -> len:int -> unit

  (** [zero_out ~pos ~len slice] sets [len] bytes of the [slice]
      to zero, beginning at byte offset [pos]. *)
  val zero_out : rw t -> pos:int -> len:int -> unit
end

module type S = sig
  module Segment : sig
    include SEGMENT
  end

  module Message : sig
    include MESSAGE with type 'a segment_t := 'a Segment.t
  end

  module Slice : sig
    include SLICE with type 'a segment_t := 'a Segment.t
                   and type 'a message_t := 'a Message.t
  end

  module StructStorage : sig
    (* Note: ['a] is marked covariant here just because it makes casting easier in some places. *)
    type ('cap, +'a) t = private { data : 'cap Slice.t; pointers : 'cap Slice.t; }
    val readonly : ('cap, 'a) t -> (ro, 'a) t
    val with_attachments : attachments -> ('cap, 'a) t -> ('cap, 'a) t
    val get_attachments : ('cap, 'a) t -> attachments
    val v : data:'cap Slice.t -> pointers:'cap Slice.t -> ('cap, 'a) t
    val cast : ('cap, 'a) t -> ('cap, 'b) t

    type 'a reader_t = (ro, 'a) t option
    type 'a builder_t = (rw, 'a) t

    val reader_of_builder : 'a builder_t -> 'a reader_t
    val message_of_builder : _ builder_t -> rw Message.t

    val cast_reader : 'a reader_t -> 'b reader_t
  end

  module ListStorage : sig
    type 'cap t = {
      storage : 'cap Slice.t;
      storage_type : ListStorageType.t;
      num_elements : int;
    }
    val readonly : 'cap t -> ro t
  end

  module Object : sig
    type ('cap, 'a) t =
      | None
      | List of 'cap ListStorage.t
      | Struct of ('cap, 'a) StructStorage.t
      | Capability of Uint32.t
  end
end

