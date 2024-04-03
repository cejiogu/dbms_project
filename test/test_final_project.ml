open OUnit2
include Final_project
include Final_project.Table

let tests_column =
  "test Column"
  >::: [
         ( "Empty Column" >:: fun _ ->
           assert_equal Column.empty (Column.make "" []) );
         ( "Add to Column" >:: fun _ ->
           assert_equal "{Time, [5:53, 2:43]}"
             (Column.string_of_column @@ Column.make "Time" [ "5:53"; "2:43" ])
         );
         ( "Empty Table" >:: fun _ ->
           assert_equal (Table.empty "test") (Table.make "test" []) );
         ( "Add String to Col" >:: fun _ ->
           assert_equal
             (Column.elemlist_of_stringlist [ "bus" ])
             (Column.data (Column.make "transport" [ "bus" ])) );
       ]

let _ = run_test_tt_main tests_column
