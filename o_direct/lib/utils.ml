
open Bigarray_compat

type t = (char, int8_unsigned_elt, c_layout) Array1.t

module RAW = struct
  (* page_alignment -> length -> t *)
  external alloc_pages: int -> int -> t = "dbutils_alloc_memaligned"

  type file_path = string
  type file_flag = bool 
  type file_perm = int
    (* open_direct path rw trunc *)
  external open_direct : file_path -> file_flag -> file_flag -> file_perm -> Unix.file_descr = "dbutils_open_direct" 

  external fdatasync : Unix.file_descr -> unit = "dbutils_fdatasync" 
end

let ftruncate fd size =
  Lwt_unix.LargeFile.ftruncate fd size

let alloc_buf ?(alignment=512) len = 
  RAW.alloc_pages alignment len |> Cstruct.of_bigarray

let open_direct = RAW.open_direct

let fdatasync = RAW.fdatasync
