let catch f c =
  try
    Lwt.catch f c
  with e -> c e
