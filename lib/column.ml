type elem =
  | NULL
  | Int of int (* 0 *)
  | Bool of bool (* 1 *)
  | Float of float (* 2 *)
  | String of string (* 3 *)
  | Date of int * int * int (* 4. Else -1 *)

type t = {
  elemtype : int;
  title : string;
  data : elem list;
}

let title t = t.title
let data t = t.data

let empty (et : int) (name : string) =
  { elemtype = et; title = name; data = [] }

let rename col new_title = { col with title = new_title }

let date_of_string (s : string) : elem option =
  (* Regular expression to match a date in the format YYYY-MM-DD *)
  let regexp =
    Str.regexp
      "\\([0-9][0-9][0-9][0-9]\\)-\\([0][1-9]\\|1[0-2]\\)-\\(0[1-9]\\|[1][0-9]\\|[2][0-9]\\|3[0-1]\\)"
  in
  if Str.string_match regexp s 0 then
    let year = Str.matched_group 1 s |> int_of_string in
    let month = Str.matched_group 2 s |> int_of_string in
    let day = Str.matched_group 3 s |> int_of_string in
    Some (Date (year, month, day))
    (* let d = Date (year, month, day) in if valid_date d then d else failwith
       "NOT A VALID DATE!" *)
  else None

let elem_of_string (s : string) : elem =
  if s = "NULL" then NULL
  else
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
            | None -> begin
                let data_type = date_of_string s in
                match data_type with
                | Some (Date (y, m, d)) -> Date (y, m, d)
                | None -> String s
                | _ ->
                    failwith "You should be getting NULL. This is impossible!"
              end
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

(* let elemtype_of_elem_num elem = match elem with | -1 -> NULL | 0 -> Int 0 | 1
   -> Bool false | 2 -> Float 0.0 | 3 -> String "" | 4 -> Date (0, 0, 0) | _ ->
   failwith "Not a possible elem number" *)

(** [elemtype_num_of_elem elem] Determine the numeric code associated with a
    specific element type.
    @param elem
      The element whose type is to be evaluated. This element can be of several
      predefined types (Int, Bool, Float, String, Date) or NULL.

    @return
      Returns a specific integer code for each
      type:
      - `-1` for NULL
      - `0` for Int
      - `1` for Bool
      - `2` for Float
      - `3` for String
      - `4`. *)
let elemtype_num_of_elem elem =
  match elem with
  | NULL -> -1
  | Int _ -> 0
  | Bool _ -> 1
  | Float _ -> 2
  | String _ -> 3
  | Date _ -> 4

(** [elemtype_num_of_data data] Finds the numeric code of the type of the first
    non-NULL element. If all elements are NULL or the list is empty, it returns
    -1.

    @param data
      The list of elements to be scanned. Each element in this list is subject
      to type evaluation, similar to what is described in
      `elemtype_num_of_elem`.

    @return
      Returns the numeric code corresponding to the type of the first non-NULL
      element found in the list according to the
      following:
      - `-1` if all elements are NULL or the list is empty.
      - An integer code (0 to 4) corresponding to the type of the first non-NULL
        element found, following the same codes as `elemtype_num_of_elem`. This
        function iterates through the list until it finds a non-NULL element or
        exhausts the list. *)

let rec elemtype_num_of_data data =
  match data with
  | [] -> -1
  | h :: t ->
      let num = elemtype_num_of_elem h in
      if num = -1 then elemtype_num_of_data t else num

let make s d =
  let data_insert = elemlist_of_stringlist d in
  { elemtype = elemtype_num_of_data data_insert; title = s; data = data_insert }

(** [string_of_date d] Converts a [Date] represented by the tuple [d] into a
    string.
    @param d
      A tuple of three ints representing a date in the format (year, month,
      day).
    @return
      A string representing the date in the format "(year, month, day)".
      Example: [string_of_date (2023, 4, 2)] returns "2023-04-02". *)
let string_of_date date =
  match date with
  | year, month, day ->
      string_of_int year ^ "-" ^ string_of_int month ^ "-" ^ string_of_int day

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

let string_of_column col =
  "{" ^ col.title ^ ", " ^ string_of_data col.data ^ "}"

let stringlist_of_data data =
  let rec aux acc lst =
    match lst with
    | [] -> List.rev acc
    | h :: t -> aux (string_of_elem h :: acc) t
  in
  aux [] data

let stringlist_of_column col = col.title :: stringlist_of_data col.data

(** [add_elem_to_column elem col] Adds a new element to the beginning of a
    column's data list.
    @param elem The element to add.
    @param col The column to which the element will be added.
    @return The updated column with the new element added. *)
let add_elem_to_column elem col =
  if elemtype_num_of_elem elem = -1 || col.elemtype = elemtype_num_of_elem elem
  then { elemtype = col.elemtype; title = col.title; data = elem :: col.data }
  else failwith "All elements must be of the same type"

let add str_elem col = add_elem_to_column (elem_of_string str_elem) col

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

(* FUNCTION CEMETERY

   let rec valid_data (data : elem list) (h_data : elem) : bool = match data
   with | [] -> true | h :: t -> begin match h_data with | NULL -> valid_data t
   h_data | Int _ -> begin match h with | NULL -> valid_data t h_data | Int _ ->
   valid_data t h_data | _ -> false end | Bool _ -> begin match h with | NULL ->
   valid_data t h_data | Bool _ -> valid_data t h_data | _ -> false end | Float
   _ -> begin match h with | NULL -> valid_data t h_data | Float _ -> valid_data
   t h_data | _ -> false end | String _ -> begin match h with | NULL ->
   valid_data t h_data | String _ -> valid_data t h_data | _ -> false end | Date
   _ -> begin match h with | NULL -> valid_data t h_data | Date _ -> valid_data
   t h_data | _ -> false end end

   let rec valid_column col = match col.data with | [] -> true | h :: t -> (
   match h with | NULL -> valid_column { elemtype = col.elemtype; title =
   col.title; data = t } | Int i -> valid_data t (Int i) | Bool b -> valid_data
   t (Bool b) | Float f -> valid_data t (Float f) | String s -> valid_data t
   (String s) | Date (y, m, d) -> valid_data t (Date (y, m, d))) *)

(* MISCELLANEOUS CEMETERY

   (** [all_numbers s] checks if the string [s] contains only numeric
   characters. @param s The string to check. @return [true] if [s] contains only
   digits; otherwise, [false]. *) let all_numbers (s : string) : bool = if
   Str.string_match (Str.regexp "[0-9]+$") s 0 then true else false

   (** [valid_year s] checks if the string [s] represents a valid year. @param s
   The string representing a year. @return [true] if [s] is "NULL" or contains
   only digits; otherwise, [false]. *) let valid_year (year : string) : bool =
   if year = "NULL" || all_numbers year then true else false

   (** [valid_month_or_day s] checks if the string [s] is a valid representation
   of a month or day. @param s The string to check. @return [true] if [s] is
   "NULL", consists of 2 digits, or both; otherwise, [false]. *) let
   valid_month_or_day (month_or_day : string) : bool = if month_or_day = "NULL"
   || (all_numbers month_or_day && String.length month_or_day = 2) then true
   else false

   (** [valid_date d] checks if the Date [d] is valid. @param d The Date to
   check, as an [elem] variant. @return [true] if [d] represents a valid Date;
   otherwise, [false]. *) let valid_date (date : elem) = match date with | Date
   (year, month, day) -> if (valid_year @@ string_of_int year) &&
   (valid_month_or_day @@ string_of_int month) && (valid_month_or_day @@
   string_of_int day) then true else false | _ -> false *)
