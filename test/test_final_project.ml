open OUnit2
include Final_project
open Final_project.Table

let tests_column =
  "test Column"
  >::: [
         ( "Empty Column" >:: fun _ ->
           assert_equal Column.empty (Column.make_column "" []) );
         ( "Add to Column" >:: fun _ ->
           assert_equal "{Time, [5:53, 2:43]}"
             (Column.string_of_column @@ Column.make_column "Time"
             @@ Column.stringlist_to_elemlist [ "5:53"; "2:43" ]) );
         ( "Empty Table" >:: fun _ ->
           assert_equal (Table.empty_table "test")
             (Table.create_table "test" []) );
         ( "Add String to Col" >:: fun _ ->
           assert_equal
             (Column.stringlist_to_elemlist [ "bus" ])
             (Column.data
                (Column.make_column "transport"
                   (Column.stringlist_to_elemlist [ "bus" ]))) );
       ]

let _ = run_test_tt_main tests_column
