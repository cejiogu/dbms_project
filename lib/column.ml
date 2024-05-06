type elem =
  | NULL
  | Int of int
  | Bool of bool
  | Float of float
  | String of string
  | Date of int * int * int

type t = {
  elemtype : elem;
  title : string;
  data : elem list;
}

let title t = t.title
let data t = t.data
let col_type t = t.elemtype
let col_size t = List.length t.data

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

let elemtype_of_stringtype str =
  match str with
  | "NULL" -> NULL
  | "Int" -> Int 0
  | "Bool" -> Bool false
  | "Float" -> Float 0.
  | "String" -> String ""
  | "Date" -> Date (0000, 01, 01)
  | _ -> failwith "Not a valid type!"

let rec elemtype_of_stringlist str_lst =
  match str_lst with
  | [] -> NULL
  | h :: t ->
      let el = elem_of_string h in
      if el = NULL then elemtype_of_stringlist t else el

(** [elem_of_elemlist elem_lst] Returns the an elem of an elemlist which
    represents what are the types of all of the elements of elem_lst. of
    elements of type [elem].
    @param elem_lst The elem list which returns the [elem]. *)
let elem_of_elemlist (lst : elem list) : elem =
  match lst with
  | [] -> NULL
  | h :: _ -> begin
      match h with
      | NULL -> NULL
      | Int _ -> Int 0
      | Bool _ -> Bool false
      | Float _ -> Float 0.
      | String _ -> String ""
      | Date _ -> Date (0000, 00, 00)
    end

let empty (name : string) (el : string) =
  { elemtype = elemtype_of_stringtype el; title = name; data = [] }

let rename col new_title = { col with title = new_title }

(** [equal el1 el2] checks if [el1] and [el2] are of the same elemtype no matter
    the values.data
    @param el1 The first elem to check.
    @param el2 The second elem to cheeck.
    @return a bool representing if the elems are of the same type. *)
let equal el1 el2 =
  match (el1, el2) with
  | Int _, Int _ -> true
  | Bool _, Bool _ -> true
  | Float _, Float _ -> true
  | String _, String _ -> true
  | Date _, Date _ -> true
  | _ -> false

(** [elemlist_of_stringlist_aux s el acc] recursively converts a list of strings
    [s] into a list of [elem]s, accumulating the result in [acc].
    @param s The list of strings to convert.
    @param el The elem type of the string list to convert.
    @param acc The accumulator for the resulting [elem] list, initially empty.
    @return
      A list of [elem]s, constructed in reverse order from the input list
      [s].

      This is a helper function designed for internal use by
      [elemlist_of_stringlist]. USE [elemlist_of_stringlist s] INSTEAD OF THIS
      FUNCTION!*)
let rec elemlist_of_stringlist_aux (s : string list) (el : elem)
    (acc : elem list) : elem list =
  match s with
  | [] -> List.rev acc
  | h :: t ->
      let elem_added = elem_of_string h in
      if equal el elem_added || elem_added = NULL then
        elemlist_of_stringlist_aux t el @@ (elem_added :: acc)
      else failwith "All elemnts must be of the same type!"

let elemlist_of_stringlist (s : string list) (el : elem) =
  elemlist_of_stringlist_aux s el []

let make s d =
  let el = elemtype_of_stringlist d in
  let data_insert = elemlist_of_stringlist d el in
  { elemtype = el; title = s; data = data_insert }

(** [add_zero s] Adds a zero to the beginning of s if the length is 1 else
    returns s
    @param s The string to add a zero to depending on the length. *)
let add_zero s = if String.length s = 1 then "0" ^ s else s

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
      string_of_int year ^ "-"
      ^ (add_zero @@ string_of_int month)
      ^ "-" ^ add_zero @@ string_of_int day

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
  if elem = NULL || equal col.elemtype elem then
    { elemtype = col.elemtype; title = col.title; data = elem :: col.data }
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

(* let print col = print_endline @@ "\n" ^ col.title; print_data col.data *)

let print col =
  print_endline col.title;
  print_data col.data

let make_raw (data : elem list) (title : string) : t =
  let col : t = { elemtype = elem_of_elemlist data; title; data } in
  col

let elemtype_of_stringparse str =
  match str with
  | "INT" -> "Int"
  | "STRING" -> "String"
  | "FLOAT" -> "Float"
  | "BOOL" -> "Bool"
  | "DATE" -> "Date"
  | _ -> failwith "Not a valid type!"

let sqlstr_of_elm = function
  | Int _ -> "INT"
  | Bool _ -> "BOOL"
  | Float _ -> "FLOAT"
  | String _ -> "STRING"
  | Date _ -> "DATE"
  | _ -> failwith "Not a valid elem!"

let string_of_elmtyp = function
  | Int _ -> "Int"
  | Bool _ -> "Bool"
  | Float _ -> "Float"
  | String _ -> "String"
  | Date _ -> "Date"
  | _ -> failwith "Not a valid elem!"

let filter_indx c indx_list =
  let l = ref [] in
  List.iter (fun x -> l := (List.nth (data c) x :: []) @ !l) indx_list;
  make_raw !l (title c)

let filter_indicies c e =
  let i = ref [] in
  for x = 0 to List.length (data c) - 1 do
    let el = List.nth (data c) x in
    if el = e then i := x :: !i else ()
  done;
  !i

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
