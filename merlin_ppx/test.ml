let%expect_test "foo" =
  print_endline "Test";
  [%expect {|Test|}];
  print_endline "Test";
  [%expect {|Test1|}];
  ()
