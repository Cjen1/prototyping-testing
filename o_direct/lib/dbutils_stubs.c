
#define _GNU_SOURCE

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>

#include <fcntl.h>
#include <string.h>
#include <unistd.h>

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/bigarray.h>
#include <caml/unixsupport.h>

#include <lwt_unix.h>

CAMLprim value dbutils_alloc_memaligned(value page_size, value len)
{
  CAMLparam2(page_size, len);

  void* block = NULL;
  int ret = posix_memalign(&block, Int_val(page_size), Int_val(len));
  if (ret < 0) {
    caml_raise_out_of_memory();
  }
//  void* block = mmap(NULL, len,
//		  PROT_READ|PROT_WRITE,
//		  MAP_PRIVATE|MAP_ANONYMOUS,
//		  -1, 0);
  memset(block, 0, len);

  CAMLreturn(caml_ba_alloc_dims(CAML_BA_CHAR | CAML_BA_C_LAYOUT | CAML_BA_MANAGED, 1, block, len));
}

CAMLprim value dbutils_open_direct(value filename, value rw, value trunc, value perm)
{
  CAMLparam4(filename, rw, trunc, perm);
  int fd;
  const char *filename_c = strdup(String_val(filename));

  enter_blocking_section();

  int flags = O_DIRECT | O_CREAT;
  if (rw) 
    flags |= O_RDWR;
  else
    flags |= O_WRONLY;
  if (trunc) 
    flags |= O_TRUNC;

  fd = open(filename_c, flags, Int_val(perm));

  leave_blocking_section();

  free((void*)filename_c);

  if (fd == -1) uerror("open", filename);

  CAMLreturn(Val_int(fd));
}
