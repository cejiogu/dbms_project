open OUnit2
include Final_project
include Final_project.Table

let tests_column =
  "test Column"
  >::: [
         (* ( "Empty Column" >:: fun _ -> assert_equal (Column.empty 0 "Test")
            (Column.make "Test" []) ); *)
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
let t1 = Table.make "IntegerTable" [ "ID"; "Value" ] [ "Int"; "Int" ]
let t1_insert1 = Table.insert_into t1 [ "ID"; "Value" ] [ "1"; "100" ]
let t1_insert2 = Table.insert_into t1_insert1 [ "ID"; "Value" ] [ "2"; "200" ]

(* Table2: *)
let t2 =
  Table.make "StringTable" [ "Name"; "Occupation" ] [ "String"; "String" ]

let t2_insert1 =
  Table.insert_into t2 [ "Name"; "Occupation" ] [ "Alice"; "Engineer" ]

let t2_insert2 =
  Table.insert_into t2_insert1 [ "Name"; "Occupation" ] [ "Bob"; "Doctor" ]

(* Table3: *)
let t3 =
  Table.make "MixedTable"
    [ "ID"; "Name"; "Birthday" ]
    [ "Int"; "String"; "Date" ]

let t3_insert1 =
  Table.insert_into t3
    [ "ID"; "Name"; "Birthday" ]
    [ "1"; "Charlie"; "1990-01-01" ]

let t3_insert2 =
  Table.insert_into t3_insert1
    [ "ID"; "Name"; "Birthday" ]
    [ "2"; "Dana"; "1985-05-23" ]

let t4 = Table.make "FloatsBools" [ "ID"; "Has_Name" ] [ "Float"; "Bool" ]
let t4_insert1 = Table.insert_into t4 [ "ID"; "Has_Name" ] [ "1.2"; "true" ]

let t4_insert2 =
  Table.insert_into t4_insert1 [ "ID"; "Has_Name" ] [ "0."; "false" ]

let t5 = Table.make "NULL_add_table" [ "ID"; "Value" ] [ "Int"; "Float" ]
let t5_insert1 = Table.insert_into t5 [ "ID"; "Value" ] [ "143"; "5.2343444" ]

let t5_insert2 =
  Table.insert_into t5_insert1 [ "ID"; "Value" ] [ "NULL"; "NULL" ]

let tests_table =
  "test Table"
  >::: [
         ( "Empty Table" >:: fun _ ->
           assert_equal (Table.empty "test") (Table.make "test" [] []) );
         ( "Make Table with only Ints (table1)" >:: fun _ ->
           assert_equal "Table: IntegerTable\n{ID, []}\n{Value, []}\n"
             (Table.string_of_table t1) );
         ( "Insert into table1" >:: fun _ ->
           assert_equal "Table: IntegerTable\n{ID, [1]}\n{Value, [100]}\n"
             (Table.string_of_table t1_insert1);
           assert_equal
             "Table: IntegerTable\n{ID, [2, 1]}\n{Value, [200, 100]}\n"
             (Table.string_of_table t1_insert2) );
         ( "Make table with only Strings (table2)" >:: fun _ ->
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
         ( "Make table with Int, String, and Date Columns (table3)" >:: fun _ ->
           assert_equal
             "Table: MixedTable\n{ID, []}\n{Name, []}\n{Birthday, []}\n"
             (Table.string_of_table t3);
           assert_equal
             "Table: MixedTable\n\
              {ID, [1]}\n\
              {Name, [Charlie]}\n\
              {Birthday, [1990-1-1]}\n"
             (Table.string_of_table t3_insert1);
           assert_equal
             "Table: MixedTable\n\
              {ID, [2, 1]}\n\
              {Name, [Dana, Charlie]}\n\
              {Birthday, [1985-5-23, 1990-1-1]}\n"
             (Table.string_of_table t3_insert2) );
         ( "Make table with Float, and Bool Columns (table4)" >:: fun _ ->
           assert_equal "Table: FloatsBools\n{ID, []}\n{Has_Name, []}\n"
             (Table.string_of_table t4);
           assert_equal "Table: FloatsBools\n{ID, [1.2]}\n{Has_Name, [true]}\n"
             (Table.string_of_table t4_insert1);
           assert_equal
             "Table: FloatsBools\n{ID, [0., 1.2]}\n{Has_Name, [false, true]}\n"
             (Table.string_of_table t4_insert2) );
         ( "Tests adding NULL (table5)" >:: fun _ ->
           assert_equal "Table: NULL_add_table\n{ID, []}\n{Value, []}\n"
             (Table.string_of_table t5);
           assert_equal
             "Table: NULL_add_table\n{ID, [143]}\n{Value, [5.2343444]}\n"
             (Table.string_of_table t5_insert1);
           assert_equal
             "Table: NULL_add_table\n\
              {ID, [NULL, 143]}\n\
              {Value, [NULL, 5.2343444]}\n"
             (Table.string_of_table t5_insert2) );
       ]

let () =
  run_test_tt_main
    ("suite"
    >::: [
           "ColumnTests" >::: [ tests_column ];
           "TableTests" >::: [ tests_table ];
         ])
