open OUnit2
include Final_project

let tests_column =
  let int_column = Column.empty "Int" "Int" in
  let bool_column = Column.empty "Bool" "Bool" in
  let float_column = Column.empty "Float" "Float" in
  let string_column = Column.empty "String" "String" in
  let date_column = Column.empty "Date" "Date" in
  "test Column"
  >::: [
         ( "Empty Column of different types" >:: fun _ ->
           assert_equal "{Int, []}" (Column.string_of_column int_column);
           assert_equal "{Bool, []}" (Column.string_of_column bool_column);
           assert_equal "{Float, []}" (Column.string_of_column float_column);
           assert_equal "{String, []}" (Column.string_of_column string_column);
           assert_equal "{Date, []}" (Column.string_of_column date_column) );
         ( "Add to Column" >:: fun _ ->
           assert_equal "{Time, [5:53, 2:43]}"
             (Column.string_of_column @@ Column.make "Time" [ "5:53"; "2:43" ]);
           assert_equal "{DATE, [2003-06-03, 2003-07-16]}"
             (Column.string_of_column
             @@ Column.make "DATE" [ "2003-06-03"; "2003-07-16" ]) );
         ( "Adding to empty columns of each type, including NULL" >:: fun _ ->
           assert_equal "{Int, [12]}"
             (Column.string_of_column @@ Column.add "12" int_column);
           assert_equal "{Int, [NULL]}"
             (Column.string_of_column @@ Column.add "NULL" int_column);
           assert_equal "{Bool, [false]}"
             (Column.string_of_column @@ Column.add "false" bool_column);
           assert_equal "{Bool, [NULL]}"
             (Column.string_of_column @@ Column.add "NULL" bool_column);
           assert_equal "{Float, [12.32]}"
             (Column.string_of_column @@ Column.add "12.32" float_column);
           assert_equal "{Float, [NULL]}"
             (Column.string_of_column @@ Column.add "NULL" float_column);
           assert_equal "{String, [HEY]}"
             (Column.string_of_column @@ Column.add "HEY" string_column);
           assert_equal "{String, [NULL]}"
             (Column.string_of_column @@ Column.add "NULL" string_column);
           assert_equal "{Date, [2012-01-29]}"
             (Column.string_of_column @@ Column.add "2012-01-29" date_column);
           assert_equal "{Date, [NULL]}"
             (Column.string_of_column @@ Column.add "NULL" date_column) );
         ( "Making column of different types" >:: fun _ ->
           assert_equal "{PI, [3, 1, 4, NULL]}"
             (Column.string_of_column
                (Column.make "PI" [ "3"; "1"; "4"; "NULL" ]));
           assert_equal "{is_column, [true, NULL, false, false]}"
             (Column.string_of_column
                (Column.make "is_column" [ "true"; "NULL"; "false"; "false" ]));
           assert_equal "{floating_pi, [3., 3.1, 3.14, NULL]}"
             (Column.string_of_column
                (Column.make "floating_pi" [ "3."; "3.1"; "3.14"; "NULL" ]));
           assert_equal
             "{New_Years, [NULL, 2022-01-01, 2023-01-01, 2024-01-01]}"
             (Column.string_of_column
                (Column.make "New_Years"
                   [ "NULL"; "2022-01-01"; "2023-01-01"; "2024-01-01" ]));
           assert_equal "{transport, [bus, car]}"
             (Column.string_of_column
                (Column.make "transport" [ "bus"; "car" ]));
           (* testing elemlist_of_stringlist function *)
           assert_equal
             (Column.elemlist_of_stringlist [ "bus"; "car" ]
                (Column.elem_of_string "String"))
             (Column.data (Column.make "transport" [ "bus"; "car" ])) );
         ( "Rename column" >:: fun _ ->
           assert_equal "{New_name, []}"
             (Column.string_of_column @@ Column.rename float_column "New_name")
         );
       ]

let tests_table =
  (* Table1: *)
  let t1 = Table.make "IntegerTable" [ "ID"; "Value" ] [ "Int"; "Int" ] in
  let t1_insert1 = Table.insert_into t1 [ "ID"; "Value" ] [ "1"; "100" ] in
  let t1_insert2 =
    Table.insert_into t1_insert1 [ "ID"; "Value" ] [ "2"; "200" ]
  in

  (* Table2: *)
  let t2 =
    Table.make "StringTable" [ "Name"; "Occupation" ] [ "String"; "String" ]
  in

  let t2_insert1 =
    Table.insert_into t2 [ "Name"; "Occupation" ] [ "Alice"; "Engineer" ]
  in

  let t2_insert2 =
    Table.insert_into t2_insert1 [ "Name"; "Occupation" ] [ "Bob"; "Doctor" ]
  in

  (* Table3: *)
  let t3 =
    Table.make "MixedTable"
      [ "ID"; "Name"; "Birthday" ]
      [ "Int"; "String"; "Date" ]
  in

  let t3_insert1 =
    Table.insert_into t3
      [ "ID"; "Name"; "Birthday" ]
      [ "1"; "Charlie"; "1990-01-01" ]
  in

  let t3_insert2 =
    Table.insert_into t3_insert1
      [ "ID"; "Name"; "Birthday" ]
      [ "2"; "Dana"; "1985-05-23" ]
  in

  let t4 = Table.make "FloatsBools" [ "ID"; "Has_Name" ] [ "Float"; "Bool" ] in
  let t4_insert1 =
    Table.insert_into t4 [ "ID"; "Has_Name" ] [ "1.2"; "true" ]
  in

  let t4_insert2 =
    Table.insert_into t4_insert1 [ "ID"; "Has_Name" ] [ "0."; "false" ]
  in

  let t5 = Table.make "NULL_add_table" [ "ID"; "Value" ] [ "Int"; "Float" ] in
  let t5_insert1 =
    Table.insert_into t5 [ "ID"; "Value" ] [ "143"; "5.2343444" ]
  in

  let t5_insert2 =
    Table.insert_into t5_insert1 [ "ID"; "Value" ] [ "NULL"; "NULL" ]
  in
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
              {Birthday, [1990-01-01]}\n"
             (Table.string_of_table t3_insert1);
           assert_equal
             "Table: MixedTable\n\
              {ID, [2, 1]}\n\
              {Name, [Dana, Charlie]}\n\
              {Birthday, [1985-05-23, 1990-01-01]}\n"
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
         ( "Rename ID column in (table5)" >:: fun _ ->
           assert_equal
             "Table: NULL_add_table\n\
              {New_Name, [NULL, 143]}\n\
              {Value, [NULL, 5.2343444]}\n"
             (Table.string_of_table
             @@ Table.rename_column "ID" "New_Name" t5_insert2) );
         ( "Testing remove column" >:: fun _ ->
           assert_equal
             "Table: NULL_add_table\n\
              {ID, [NULL, 143]}\n\
              {Value, [NULL, 5.2343444]}\n"
             (Table.string_of_table t5_insert2);
           assert_equal "Table: NULL_add_table\n{Value, [NULL, 5.2343444]}\n"
             (Table.string_of_table @@ Table.remove "ID" t5_insert2);
           assert_equal
             "Table: NULL_add_table\n\
              {ID, [NULL, 143]}\n\
              {Value, [NULL, 5.2343444]}\n"
             (Table.string_of_table @@ Table.remove "NOTINTABLE" t5_insert2) );
         ( "Test alter table" >:: fun _ ->
           assert_equal
             (Table.make "test"
                [ "testa"; "testb"; "testc"; "testd" ]
                [ "Int"; "Float"; "Date"; "Bool" ])
             (Table.alter_table_add
                (Table.make "test"
                   [ "testa"; "testb"; "testc" ]
                   [ "Int"; "Float"; "Date" ])
                "testd" "Bool");
           assert_equal
             (Table.make "test"
                [ "testa"; "testb"; "testc"; "testb" ]
                [ "Int"; "Float"; "Date"; "Float" ])
             (Table.alter_table_add
                (Table.make "test"
                   [ "testa"; "testb"; "testc" ]
                   [ "Int"; "Float"; "Date" ])
                "testb" "Float") );
         ( "Test insert_col" >:: fun _ ->
           assert_equal
             (Table.string_of_table
                (Table.insert_col (Table.empty "test")
                   (Column.make "testa" [ "testelma"; "testelmb" ])))
             "Table: test\n{testa, [testelma, testelmb]}\n"
             ~printer:(fun x -> x) );
       ]

let tests_database =
  let empt_database = Database.empty "testa" in
  let table_snk =
    Table.insert_into
      (Table.insert_into
         (Table.insert_into
            (Table.insert_into
               (Table.make "sneakers"
                  [ "Brand"; "Model"; "Units"; "Price" ]
                  [ "String"; "String"; "Int"; "Float" ])
               [ "Brand"; "Model"; "Units"; "Price" ]
               [ "Nike"; "Air Force 1"; "10000000"; "110.00" ])
            [ "Brand"; "Model"; "Units"; "Price" ]
            [ "Nike"; "Air Jordan 1"; "120000"; "170.00" ])
         [ "Brand"; "Model"; "Units"; "Price" ]
         [ "Nike"; "Air Jordan 4"; "80000"; "210.00" ])
      [ "Brand"; "Model"; "Units"; "Price" ]
      [ "Nike"; "Air\n   Jordan 3"; "40000"; "200.00" ]
  in

  let table_flights =
    Table.insert_into
      (Table.insert_into
         (Table.insert_into
            (Table.insert_into
               (Table.make "flights"
                  [ "Airline"; "Origin"; "Destination"; "Duration" ]
                  [ "String"; "String"; "String"; "Float" ])
               [ "Airline"; "Origin"; "Destination"; "Duration" ]
               [ "Delta"; "NYC"; "BOS"; "1.0" ])
            [ "Airline"; "Origin"; "Destination"; "Duration" ]
            [ "United"; "NYC"; "LAX"; "6.0" ])
         [ "Airline"; "Origin"; "Destination"; "Duration" ]
         [ "American Airlines"; "BOS"; "MIA"; "2.9" ])
      [ "Airline"; "Origin"; "Destination"; "Duration" ]
      [ "Delta"; "NYC"; "LON"; "6.0" ]
  in

  let table_flights_sub =
    Table.insert_into
      (Table.insert_into
         (Table.make "flights"
            [ "Airline"; "Origin"; "Destination" ]
            [ "String"; "String"; "String" ])
         [ "Airline"; "Origin"; "Destination" ]
         [ "United"; "NYC"; "LAX" ])
      [ "Airline"; "Origin"; "Destination" ]
      [ "Delta"; "NYC"; "LON" ]
  in

  let table_train =
    Table.insert_into
      (Table.make "trains"
         [ "Company"; "Origin"; "Destination"; "Duration" ]
         [ "String"; "String"; "String"; "Float" ])
      [ "Company"; "Origin"; "Destination"; "Duration" ]
      [ "Amtrak"; "NYC"; "BOS"; "3.5" ]
  in

  let full_database =
    Database.add
      (Database.add (Database.add empt_database table_snk) table_flights)
      table_train
  in
  "test Database"
  >::: [
         ( "Empty Database Name" >:: fun _ ->
           assert_equal "testa" (Database.name empt_database) );
         ( "Empty Database exists" >:: fun _ ->
           assert_equal false (Database.table_exists "trains" empt_database) );
         ( "Database exists" >:: fun _ ->
           assert_equal true
             (Database.table_exists "trains"
                (Database.add empt_database table_train)) );
         ( "Database exists (false after delete)" >:: fun _ ->
           assert_equal false
             (Database.table_exists "trains"
                (Database.delete
                   (Database.add empt_database table_train)
                   table_train)) );
         ( "Database get & insert table test" >:: fun _ ->
           assert_equal
             (Table.make "test" [ "cola"; "colb"; "colc" ]
                [ "Int"; "Bool"; "Float" ])
             ((Database.get_table
                 (Database.insert_table empt_database "test"
                    [ "cola"; "colb"; "colc" ] [ "Int"; "Bool"; "Float" ]))
                "test") );
         ( "Database get & insert table test" >:: fun _ ->
           assert_equal
             (Table.make "test" [ "cola"; "colb"; "colc" ]
                [ "Int"; "Bool"; "Float" ])
             ((Database.get_table
                 (Database.insert_table empt_database "test"
                    [ "cola"; "colb"; "colc" ] [ "Int"; "Bool"; "Float" ]))
                "test") );
         ( "Database select_from_where" >:: fun _ ->
           assert_equal table_flights_sub
             (Database.select_from_where full_database
                [ "Airline"; "Origin"; "Destination" ]
                "flights" ("Duration", "6.0"))
             ~printer:(fun x -> Table.string_of_table x) );
         ( "Database select_from_where first col" >:: fun _ ->
           assert_equal
             (Table.insert_into
                (Table.insert_into
                   (Table.make "flights" [ "Airline" ] [ "String" ])
                   [ "Airline" ] [ "United" ])
                [ "Airline" ] [ "Delta" ])
             (Database.select_from_where full_database [ "Airline" ] "flights"
                ("Duration", "6.0"))
             ~printer:(fun x -> Table.string_of_table x) );
         ( "Database select_from_where middle col" >:: fun _ ->
           assert_equal
             (Table.insert_into
                (Table.insert_into
                   (Table.make "flights" [ "Origin" ] [ "String" ])
                   [ "Origin" ] [ "NYC" ])
                [ "Origin" ] [ "NYC" ])
             (Database.select_from_where full_database [ "Origin" ] "flights"
                ("Duration", "6.0"))
             ~printer:(fun x -> Table.string_of_table x) );
       ]

let tests_equal_function =
  let open Final_project in
  (* Setup test tables *)
  let table1 = Table.make "TestTable" [ "ID"; "Name" ] [ "Int"; "String" ] in
  let table1 = Table.insert_into table1 [ "ID"; "Name" ] [ "1"; "Alice" ] in
  let table1 = Table.insert_into table1 [ "ID"; "Name" ] [ "2"; "Bob" ] in

  let table2 = Table.make "TestTable" [ "ID"; "Name" ] [ "Int"; "String" ] in
  let table2 = Table.insert_into table2 [ "ID"; "Name" ] [ "1"; "Alice" ] in
  let table2 = Table.insert_into table2 [ "ID"; "Name" ] [ "2"; "Bob" ] in

  let table3 = Table.make "TestTable" [ "ID"; "Name" ] [ "Int"; "String" ] in
  let table3 = Table.insert_into table3 [ "ID"; "Name" ] [ "1"; "Alice" ] in

  let table4 =
    Table.make "DifferentTestTable" [ "ID"; "Name" ] [ "Int"; "String" ]
  in

  let table5 = Table.make "TestTable" [ "ID"; "Age" ] [ "Int"; "Int" ] in

  let table6 = Table.make "TestTable" [ "ID"; "Name" ] [ "Int"; "String" ] in
  let table6 = Table.insert_into table6 [ "ID"; "Name" ] [ "1"; "Alice" ] in
  let table6 = Table.insert_into table6 [ "ID"; "Name" ] [ "2"; "Bob" ] in
  let table6 = Table.insert_into table6 [ "ID"; "Name" ] [ "3"; "Charlie" ] in

  (* Test for complete equality *)
  "test Table equality"
  >::: [
         ( "Equal Tables" >:: fun _ ->
           assert_bool "Tables should be equal" (Table.equal table1 table2) );
         ( "Unequal Tables - Less Data" >:: fun _ ->
           assert_bool "Tables should not be equal"
             (not (Table.equal table1 table3)) );
         ( "Same Table Reference" >:: fun _ ->
           assert_bool "Tables should be equal" (Table.equal table1 table1) );
         ( "Different names" >:: fun _ ->
           assert_bool "Tables with different names should not be equal"
             (not (Table.equal table1 table4)) );
         ( "Different columns" >:: fun _ ->
           assert_bool "Tables with different columns should not be equal"
             (not (Table.equal table1 table5)) );
         ( "Different data" >:: fun _ ->
           assert_bool "Tables with different data should not be equal"
             (not (Table.equal table1 table6)) );
       ]

let tests_inner_join =
  let setup_tables () =
    (* Create the first table with employees *)
    let table1 = Table.make "Employees" [ "ID"; "Name" ] [ "Int"; "String" ] in
    let table1 = Table.insert_into table1 [ "ID"; "Name" ] [ "1"; "Alice" ] in
    let table1 = Table.insert_into table1 [ "ID"; "Name" ] [ "2"; "Bob" ] in
    let table1 = Table.insert_into table1 [ "ID"; "Name" ] [ "3"; "Charlie" ] in

    (* Create the second table with departments *)
    let table2 =
      Table.make "Departments" [ "ID"; "Dept" ] [ "Int"; "String" ]
    in
    let table2 = Table.insert_into table2 [ "ID"; "Dept" ] [ "2"; "HR" ] in
    let table2 =
      Table.insert_into table2 [ "ID"; "Dept" ] [ "3"; "Engineering" ]
    in
    let table2 =
      Table.insert_into table2 [ "ID"; "Dept" ] [ "4"; "Marketing" ]
    in
    (table1, table2)
  in

  "test Inner Join"
  >::: [
         ( "Normal Case" >:: fun _ ->
           let table1, table2 = setup_tables () in
           let result_table = Table.inner_join table1 table2 "ID" in
           let expected_columns = [ "ID"; "Name"; "Dept" ] in
           let expected_types = [ "Int"; "String"; "String" ] in
           let expected_table =
             Table.make "Joined_Employees_Departments" expected_columns
               expected_types
           in
           let expected_table =
             List.fold_left
               (fun acc (id, name, dept) ->
                 Table.insert_into acc [ "ID"; "Name"; "Dept" ]
                   [ id; name; dept ])
               expected_table
               [ ("3", "Charlie", "Engineering"); ("2", "Bob", "HR") ]
           in
           assert_equal ~cmp:Table.equal ~printer:Table.string_of_table
             expected_table result_table );
         ( "No Match Case" >:: fun _ ->
           let table1 =
             Table.make "Employees" [ "ID"; "Name" ] [ "Int"; "String" ]
           in
           let table1 =
             Table.insert_into table1 [ "ID"; "Name" ] [ "1"; "Alice" ]
           in
           let table1 =
             Table.insert_into table1 [ "ID"; "Name" ] [ "2"; "Bob" ]
           in

           let table2 =
             Table.make "Departments" [ "ID"; "Dept" ] [ "Int"; "String" ]
           in
           let table2 =
             Table.insert_into table2 [ "ID"; "Dept" ] [ "3"; "HR" ]
           in
           let table2 =
             Table.insert_into table2 [ "ID"; "Dept" ] [ "4"; "Engineering" ]
           in
           let result_table = Table.inner_join table1 table2 "ID" in
           assert_equal
             "Table: Joined_Employees_Departments\n\
              {ID, []}\n\
              {Name, []}\n\
              {Dept, []}\n"
             (Table.string_of_table result_table) );
         ( "Nonexistent Column" >:: fun _ ->
           let table1, table2 = setup_tables () in
           let run_test () = Table.inner_join table1 table2 "Nonexistent" in
           assert_raises
             (Table.InvalidQuery
                "Column Nonexistent not found in one or both tables") run_test
         );
         ( "Empty Tables" >:: fun _ ->
           let empty_table1 = Table.empty "Empty1" in
           let empty_table2 = Table.empty "Empty2" in
           let result_table () =
             Table.inner_join empty_table1 empty_table2 "ID"
           in
           assert_raises
             (Table.InvalidQuery "Column ID not found in one or both tables")
             result_table );
       ]

let tests_printing =
  let print_column = Column.make "Print Column" [ "1"; "2"; "3"; "4" ] in
  let print_table =
    let t =
      Table.make "Print Table"
        [ "Strings"; "Ints"; "Bools"; "Dates"; "Floates" ]
        [ "String"; "Int"; "Bool"; "Date"; "Float" ]
    in
    let t1 =
      Table.insert_into t
        [ "Strings"; "Ints"; "Bools"; "Dates"; "Floates" ]
        [ "Print that stuff well"; "5"; "false"; "2003-07-16"; "3.14159262" ]
    in
    Table.insert_into t1
      [ "Strings"; "Ints"; "Bools"; "Dates"; "Floates" ]
      [ "Print that stuff even better"; "100"; "true"; "2023-12-31"; "2.1" ]
  in
  "test print functions"
  >::: [
         ( "Print\n   column" >:: fun _ ->
           assert_equal () (Column.print print_column) );
         ( "Testing print table" >:: fun _ ->
           assert_equal () (Table.print print_table) );
       ]

let () =
  run_test_tt_main
    ("suite"
    >::: [
           "ColumnTests" >::: [ tests_column ];
           "TableTests" >::: [ tests_table ];
           "DatabaseTests" >::: [ tests_database ];
           "Table Equality Tests" >::: [ tests_equal_function ];
           "tests inner_join of tables" >::: [ tests_inner_join ];
           "PrintingTests" >::: [ tests_printing ];
         ])
