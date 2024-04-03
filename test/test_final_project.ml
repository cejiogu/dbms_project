open OUnit2
include Final_project
include Final_project.Table

let tests_column =
  "test Column"
  >::: [
         ( "Empty Column" >:: fun _ ->
           assert_equal (Column.empty "") (Column.make "" []) );
         ( "Add to Column" >:: fun _ ->
           assert_equal "{Time, [5:53, 2:43]}"
             (Column.string_of_column @@ Column.make "Time" [ "5:53"; "2:43" ])
         );
         ( "Add String to Col" >:: fun _ ->
           assert_equal
             (Column.elemlist_of_stringlist [ "bus" ])
             (Column.data (Column.make "transport" [ "bus" ])) );
       ]

let t1 = Table.make "IntegerTable" [ "ID"; "Value" ]
let t1_insert1 = Table.insert_into t1 [ "ID"; "Value" ] [ "1"; "100" ]
let t1_insert2 = Table.insert_into t1_insert1 [ "ID"; "Value" ] [ "2"; "200" ]

(* let table2 = let t = Table.make "StringTable" [ "Name"; "Occupation" ] in let
   t1 = Table.insert_into t [ "Name"; "Occupation" ] [ "Alice"; "Engineer" ] in
   Table.insert_into t1 [ "Name"; "Occupation" ] [ "Bob"; "Doctor" ]

   let table3 = let t = Table.make "MixedTable" [ "ID"; "Name"; "Birthday" ] in
   let t1 = Table.insert_into t [ "ID"; "Name"; "Birthday" ] [ "1"; "Charlie";
   "1990-01-01" ] in Table.insert_into t1 [ "ID"; "Name"; "Birthday" ] [ "2";
   "Dana"; "1985-05-23" ] *)

let tests_table =
  "test Table"
  >::: [
         ( "Empty Table" >:: fun _ ->
           assert_equal (Table.empty "test") (Table.make "test" []) );
         ( "Make Table with Columns (table1)" >:: fun _ ->
           assert_equal "Table: IntegerTable\n{ID, []}\n{Value, []}\n"
             (Table.string_of_table t1) );
         ( "Insert into table1" >:: fun _ ->
           assert_equal "Table: IntegerTable\n{ID, [1]}\n{Value, [100]}\n"
             (Table.string_of_table t1_insert1) );
         ( "Insert into table1 again" >:: fun _ ->
           assert_equal
             "Table: IntegerTable\n{ID, [2, 1]}\n{Value, [200, 100]}\n"
             (Table.string_of_table t1_insert2) );
       ]

let _ = run_test_tt_main tests_column
let _ = run_test_tt_main tests_table
