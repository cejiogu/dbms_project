open OUnit2
include Final_project

let tests_Cell =
  "test suite"
  >::: [
         ( "Empty Column" >:: fun _ ->
           assert_equal Column.empty (Column.make_column "" []) );
       ]

let _ = run_test_tt_main tests_Cell
