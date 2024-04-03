open OUnit2
include Final_project
include Final_project.Table

let tests_column =
  "test Column"
  >::: [
         ( "Empty Column" >:: fun _ ->
           assert_equal (Column.empty "empty") (Column.make "empty" []) );
         ( "Add to Column" >:: fun _ ->
           assert_equal "{Time, [5:53, 2:43]}"
             (Column.string_of_column @@ Column.make "Time" [ "5:53"; "2:43" ])
         );
         ( "Add String to Col" >:: fun _ ->
           assert_equal
             (Column.elemlist_of_stringlist [ "bus" ])
             (Column.data (Column.make "transport" [ "bus" ])) );
       ]

(* Table1: *)
let t1 = Table.make "IntegerTable" [ "ID"; "Value" ]
let t1_insert1 = Table.insert_into t1 [ "ID"; "Value" ] [ "1"; "100" ]
let t1_insert2 = Table.insert_into t1_insert1 [ "ID"; "Value" ] [ "2"; "200" ]

(* Table2: *)
let t2 = Table.make "StringTable" [ "Name"; "Occupation" ]

let t2_insert1 =
  Table.insert_into t2 [ "Name"; "Occupation" ] [ "Alice"; "Engineer" ]

let t2_insert2 =
  Table.insert_into t2_insert1 [ "Name"; "Occupation" ] [ "Bob"; "Doctor" ]

(* Table3: *)
let t3 = Table.make "MixedTable" [ "ID"; "Name"; "Birthday" ]

let t3_insert1 =
  Table.insert_into t3
    [ "ID"; "Name"; "Birthday" ]
    [ "1"; "Charlie"; "1990-01-01" ]

let t3_insert2 =
  Table.insert_into t3_insert1
    [ "ID"; "Name"; "Birthday" ]
    [ "2"; "Dana"; "1985-05-23" ]

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
             (Table.string_of_table t1_insert1);
           assert_equal
             "Table: IntegerTable\n{ID, [2, 1]}\n{Value, [200, 100]}\n"
             (Table.string_of_table t1_insert2) );
         ( "Table2 test" >:: fun _ ->
           assert_equal "Table: StringTable\n{Name, []}\n{Occupation, []}\n"
             (Table.string_of_table t2);
           assert_equal
             "Table: StringTable\n{Name, [Alice]}\n{Occupation, [Engineer]}\n"
             (Table.string_of_table t2_insert1);
           assert_equal
             "Table: StringTable\n\
              {Name, [Bob, Alice]}\n\
              {Occupation, [Doctor, Engineer]}\n"
             (Table.string_of_table t2_insert2) );
         ( "Table3 test" >:: fun _ ->
           assert_equal
             "Table: MixedTable\n{ID, []}\n{Name, []}\n{Birthday, []}\n"
             (Table.string_of_table t3);
           assert_equal
             "Table: MixedTable\n\
              {ID, [1]}\n\
              {Name, [Charlie]}\n\
              {Birthday, [1990-01-01]}\n"
             (Table.string_of_table t3_insert1);
           assert_equal
             "Table: MixedTable\n\
              {ID, [2, 1]}\n\
              {Name, [Dana, Charlie]}\n\
              {Birthday, [1985-05-23, 1990-01-01]}\n"
             (Table.string_of_table t3_insert2) );
       ]

let () =
  run_test_tt_main
    ("suite"
    >::: [
           "ColumnTests" >::: [ tests_column ];
           "TableTests" >::: [ tests_table ];
         ])
