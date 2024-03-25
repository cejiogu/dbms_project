open OUnit2
include Final_project

let tests_Cell =
  "test suite" >::: [ ("Empty Table" >:: fun _ -> assert_equal [] []) ]

let _ = run_test_tt_main tests_Cell
