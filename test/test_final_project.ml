open OUnit2
include Final_project

let tests_column =
  "test Column"
  >::: [
         ( "Empty Column" >:: fun _ ->
           assert_equal Column.empty (Column.make_column "" []) );
         ( "Add to Column" >:: fun _ ->
           assert_equal "{Time, [5:53, 2:43]}"
             (Column.string_of_column
             @@ Column.make_column "Time" [ "5:53"; "2:43" ]) );
       ]

let _ = run_test_tt_main tests_column
