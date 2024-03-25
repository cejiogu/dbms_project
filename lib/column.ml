include String

type elem =
  | Int of int
  | Bool of bool
  | Float of float
  | String of string
  | Date of string * string * string

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
  | Int _ -> false
  | Bool _ -> false
  | Float _ -> false
  | String _ -> false
  | Date (year, month, day) ->
      if
        is_valid_year year
        && is_valid_month_or_day month
        && is_valid_month_or_day day
      then true
      else false

let label t = t.label
let data t = t.data
