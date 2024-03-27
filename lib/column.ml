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

let label t = t.label
let data t = t.data
