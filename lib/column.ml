type elem =
  | NULL
  | Int of int
  | Bool of bool
  | Float of float
  | String of string
  | Date of int * int * int

type t = {
  title : string;
  data : elem list;
}

(** [all_numbers s] checks if the string [s] contains only numeric characters.
    @param s The string to check.
    @return [true] if [s] contains only digits; otherwise, [false]. *)
let all_numbers (s : string) : bool =
  if Str.string_match (Str.regexp "[0-9]+$") s 0 then true else false

(** [valid_year s] checks if the string [s] represents a valid year.
    @param s The string representing a year.
    @return [true] if [s] is "NULL" or contains only digits; otherwise, [false]. *)
let valid_year (year : string) : bool =
  if year = "NULL" || all_numbers year then true else false

(** [valid_month_or_day s] checks if the string [s] is a valid representation of
    a month or day.
    @param s The string to check.
    @return
      [true] if [s] is "NULL", consists of 2 digits, or both; otherwise,
      [false]. *)
let valid_month_or_day (month_or_day : string) : bool =
  if
    month_or_day = "NULL"
    || (all_numbers month_or_day && String.length month_or_day = 2)
  then true
  else false

(** [valid_date d] checks if the Date [d] is valid.
    @param d The Date to check, as an [elem] variant.
    @return [true] if [d] represents a valid Date; otherwise, [false]. *)
let valid_date (date : elem) =
  match date with
  | Date (year, month, day) ->
      if
        (valid_year @@ string_of_int year)
        && (valid_month_or_day @@ string_of_int month)
        && (valid_month_or_day @@ string_of_int day)
      then true
      else false
  | _ -> false

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

let empty = { title = ""; data = [] }

let elem_of_string (s : string) : elem =
  let data_type = int_of_string_opt s in
  match data_type with
  | Some int -> Int int
  | None -> begin
      let data_type = float_of_string_opt s in
      match data_type with
      | Some float -> Float float
      | None -> begin
          let data_type = bool_of_string_opt s in
          match data_type with
          | Some bool -> Bool bool
          | None ->
              let data_type = date_of_string s in
              if data_type = NULL then String s else data_type
        end
    end

(** [elemlist_of_stringlist_aux s acc] recursively converts a list of strings
    [s] into a list of [elem]s, accumulating the result in [acc].
    @param s The list of strings to convert.
    @param acc The accumulator for the resulting [elem] list, initially empty.
    @return
      A list of [elem]s, constructed in reverse order from the input list
      [s].

      This is a helper function designed for internal use by
      [elemlist_of_stringlist]. USE [elemlist_of_stringlist s] INSTEAD OF THIS
      FUNCTION!*)
let rec elemlist_of_stringlist_aux (s : string list) (acc : elem list) :
    elem list =
  match s with
  | [] -> List.rev acc
  | h :: t -> elemlist_of_stringlist_aux t @@ (elem_of_string h :: acc)

let elemlist_of_stringlist (s : string list) = elemlist_of_stringlist_aux s []

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
      | NULL -> valid_column { title = col.title; data = t }
      | Int i -> valid_data t (Int i)
      | Bool b -> valid_data t (Bool b)
      | Float f -> valid_data t (Float f)
      | String s -> valid_data t (String s)
      | Date (y, m, d) -> valid_data t (Date (y, m, d)))

let make s d = { title = s; data = elemlist_of_stringlist d }
let add_elem_to_column elem col = { title = col.title; data = elem :: col.data }
let title t = t.title
let data t = t.data

(** [string_of_date d] Converts a [Date] represented by the tuple [d] into a
    string.
    @param d
      A tuple of three ints representing a date in the format (year, month,
      day).
    @return
      A string representing the date in the format "(year, month, day)".
      Example: [string_of_date (2023, 4, 2)] returns "(2023, 4, 2)". *)
let string_of_date date =
  match date with
  | year, month, day ->
      "(" ^ string_of_int year ^ ", " ^ string_of_int month ^ ", "
      ^ string_of_int day ^ ")"

let string_of_elem (e : elem) : string =
  match e with
  | NULL -> "NULL"
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b
  | Float f -> string_of_float f
  | String s -> s
  | Date (y, m, d) -> string_of_date (y, m, d)

(** [string_of_data d] converts a list of elements [d] into a string
    representation.

    @param d The elem list to be converted to a string representation.
    @return
      A string representation of the list in the format
      "[elem1, elem2, ..., elemN]".

      Example: [string_of_data [1; 2; 3]] returns "[[1, 2, 3]]". *)
let string_of_data data =
  let rec aux acc = function
    | [] -> acc
    | [ last ] ->
        acc ^ string_of_elem last (* No comma after the last element *)
    | h :: t -> aux (acc ^ string_of_elem h ^ ", ") t
  in
  "[" ^ aux "" data ^ "]"

let stringlist_of_data data =
  let rec aux acc lst =
    match lst with
    | [] -> List.rev acc
    | h :: t -> aux (string_of_elem h :: acc) t
  in
  aux [] data

let stringlist_of_column col = col.title :: stringlist_of_data col.data

let string_of_column col =
  "{" ^ col.title ^ ", " ^ string_of_data col.data ^ "}"

(** [print_data d] prints each element of the list [d] on a new line.
    @param d The elem list to print. *)
let rec print_data data =
  match data with
  | [] -> ()
  | h :: t ->
      print_endline @@ string_of_elem h;
      print_data t

let print col =
  print_endline col.title;
  print_data col.data
