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

(* [all_numbers s] returns whether or not the string [s] contains only
   numbers. *)
let all_numbers (s : string) : bool =
  if Str.string_match (Str.regexp "[0-9]+$") s 0 then true else false

(* [is_valid_year s] returns whether or not the string [s] is a valid year. *)
let is_valid_year (year : string) : bool =
  if year = "NULL" || all_numbers year then true else false

(* [is_valid_month_or_day s] returns whether or not the string [s] is a valid
   month or day. *)
let is_valid_month_or_day (month_or_day : string) : bool =
  if
    month_or_day = "NULL"
    || (all_numbers month_or_day && String.length month_or_day = 2)
  then true
  else false

(* [is_valid_date d] returns whether or not the Date [d] is a valid Date. *)
let is_valid_date (date : elem) =
  match date with
  | Date (year, month, day) ->
      if
        (is_valid_year @@ string_of_int year)
        && (is_valid_month_or_day @@ string_of_int month)
        && (is_valid_month_or_day @@ string_of_int day)
      then true
      else false
  | _ -> false

(*[date_of_string s] takes in a string [s] and returns [s] as an elem of type
  [Date]. If [s] cannot be returned as a [Date] then [NULL] is returned. *)
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
    Date (year, month, day)
  else NULL

let empty = { label = ""; data = [] }

(* [string_to_elem s] takes in a string [s] and returns [s] as an [elem]. *)
let string_to_elem (s : string) : elem =
  try Int (int_of_string s)
  with Failure _ -> (
    try Bool (bool_of_string s)
    with Failure _ -> (
      try Float (float_of_string s)
      with Failure _ -> (
        try String s
        with Failure _ -> ( try date_of_string s with Failure _ -> NULL))))

(* [stringlist_to_elemlist_aux s acc] is a helper function for
   [stringlist_to_elemlist]. *)
let rec stringlist_to_elemlist_aux (s : string list) (acc : elem list) :
    elem list =
  match s with
  | [] -> List.rev acc
  | h :: t -> stringlist_to_elemlist_aux t @@ (string_to_elem h :: acc)

(** [stringlist_to_elemlist s] returns the string [s] as an [elem list]. *)
let stringlist_to_elemlist (s : string list) = stringlist_to_elemlist_aux s []

(* [valid_data d h] takes in an elem list [d] which could be the data of a
   column. [h] is the elem type that [d] should be throughout. Returns true if
   [d] is all of type [h] otherwise returns false. *)
let rec valid_data (data : elem list) (h_data : elem) : bool =
  match data with
  | [] -> true
  | h :: t -> begin
      match h_data with
      | NULL -> valid_data t h_data
      | Int _ -> begin
          match h with
          | NULL -> valid_data t h_data
          | Int _ -> valid_data t h_data
          | _ -> false
        end
      | Bool _ -> begin
          match h with
          | NULL -> valid_data t h_data
          | Bool _ -> valid_data t h_data
          | _ -> false
        end
      | Float _ -> begin
          match h with
          | NULL -> valid_data t h_data
          | Float _ -> valid_data t h_data
          | _ -> false
        end
      | String _ -> begin
          match h with
          | NULL -> valid_data t h_data
          | String _ -> valid_data t h_data
          | _ -> false
        end
      | Date _ -> begin
          match h with
          | NULL -> valid_data t h_data
          | Date _ -> valid_data t h_data
          | _ -> false
        end
    end

let rec valid_column col =
  match col.data with
  | [] -> true
  | h :: t -> (
      match h with
      | NULL -> valid_column { label = col.label; data = t }
      | Int i -> valid_data t (Int i)
      | Bool b -> valid_data t (Bool b)
      | Float f -> valid_data t (Float f)
      | String s -> valid_data t (String s)
      | Date (y, m, d) -> valid_data t (Date (y, m, d)))

let make_column s d = { label = s; data = d }
let add_elem_to_column elem col = { label = col.label; data = elem :: col.data }
let label t = t.label
let data t = t.data

let string_of_data date =
  match date with
  | year, month, day ->
      "(" ^ string_of_int year ^ ", " ^ string_of_int month ^ ", "
      ^ string_of_int day ^ ")"

let elem_to_string (e : elem) : string =
  match e with
  | NULL -> "NULL"
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b
  | Float f -> string_of_float f
  | String s -> s
  | Date (y, m, d) -> string_of_data (y, m, d)

let rec print_data data =
  match data with
  | [] -> ()
  | h :: t ->
      print_endline @@ elem_to_string h;
      print_data t

let print col =
  print_endline col.label;
  print_data col.data
