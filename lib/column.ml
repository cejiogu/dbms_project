type elem =
  | NULL
  | Int of int
  | Bool of bool
  | Float of float
  | String of string
  | Date of int * int * int

type t = {
  label : string;
  data : elem list;
}

let label t = t.label
let data t = t.data

let all_numbers (s : string) : bool =
  if Str.string_match (Str.regexp "[0-9]+$") s 0 then true else false

let is_valid_year (year : string) : bool =
  if year = "NULL" || all_numbers year then true else false

let is_valid_month_or_day (month_or_day : string) : bool =
  if
    month_or_day = "NULL"
    || (all_numbers month_or_day && String.length month_or_day = 2)
  then true
  else false

let is_valid_date (date : elem) =
  match date with
  | NULL -> false
  | Int _ -> false
  | Bool _ -> false
  | Float _ -> false
  | String _ -> false
  | Date (year, month, day) ->
      if
        (is_valid_year @@ string_of_int year)
        && (is_valid_month_or_day @@ string_of_int month)
        && (is_valid_month_or_day @@ string_of_int day)
      then true
      else false

let date_of_string (s : string) : elem =
  (* Regular expression to match a date in the format YYYY-MM-DD *)
  let regexp =
    Str.regexp
      "^\\(\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)\\)$"
  in
  if Str.string_match regexp s 0 then
    let year = Str.matched_group 1 s |> int_of_string in
    let month = Str.matched_group 2 s |> int_of_string in
    let day = Str.matched_group 3 s |> int_of_string in
    (* Now we can validate year, month, and day if needed *)
    Date (year, month, day)
  else NULL

let empty () = { label = ""; data = [] }

let string_to_elem (s : string) : elem =
  try Int (int_of_string s)
  with Failure _ -> (
    try Bool (bool_of_string s)
    with Failure _ -> (
      try Float (float_of_string s)
      with Failure _ -> (
        try String s
        with Failure _ -> ( try date_of_string s with Failure _ -> NULL))))

let make_column s d = { label = s; data = d }

let rec valid_column_aux (data : elem list) (h_data : elem) : bool =
  match data with
  | [] -> true
  | h :: t -> begin
      match h_data with
      | NULL -> begin
          match h with
          | NULL -> valid_column_aux t h_data
          | Int _ -> valid_column_aux t h_data
          | Bool _ -> valid_column_aux t h_data
          | Float _ -> valid_column_aux t h_data
          | String _ -> valid_column_aux t h_data
          | Date _ -> valid_column_aux t h_data
        end
      | Int _ -> begin
          match h with
          | NULL -> valid_column_aux t h_data
          | Int _ -> valid_column_aux t h_data
          | Bool _ -> false
          | Float _ -> false
          | String _ -> false
          | Date _ -> false
        end
      | Bool _ -> begin
          match h with
          | NULL -> valid_column_aux t h_data
          | Int _ -> false
          | Bool _ -> valid_column_aux t h_data
          | Float _ -> false
          | String _ -> false
          | Date _ -> false
        end
      | Float _ -> begin
          match h with
          | NULL -> valid_column_aux t h_data
          | Int _ -> false
          | Bool _ -> false
          | Float _ -> valid_column_aux t h_data
          | String _ -> false
          | Date _ -> false
        end
      | String _ -> begin
          match h with
          | NULL -> valid_column_aux t h_data
          | Int _ -> false
          | Bool _ -> false
          | Float _ -> false
          | String _ -> valid_column_aux t h_data
          | Date _ -> false
        end
      | Date _ -> begin
          match h with
          | NULL -> valid_column_aux t h_data
          | Int _ -> false
          | Bool _ -> false
          | Float _ -> false
          | String _ -> false
          | Date _ -> valid_column_aux t h_data
        end
    end

let rec valid_column col =
  match col.data with
  | [] -> true
  | h :: _ -> valid_column_aux col.data h
