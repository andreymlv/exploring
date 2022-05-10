(* Homework #1 for Coursera course Programming Languages: Part A. 
   To test this type in sml promt: - use "dates_tests.sml"; *)

(* Evaluates to true if the first argument is a date that comes before the second argument.
   If the two dates are the same,the result is false. *)
fun is_older ((y1, m1, d1), (y2, m2, d2)) =
  let
    fun cmp (v1, v2, cmpnext) = 
      case Int.compare (v1, v2) of
        EQUAL => cmpnext ()
      | r => r
  in
    cmp (y1, y2,
    fn _ => cmp (m1, m2,
    fn _ => cmp (d1, d2,
    fn _ => EQUAL))) = LESS
  end

(* Produce how many dates in the list are in the given month. *)
fun number_in_month (dates: (int * int * int) list, month) = 
  length (List.filter
    (fn (date) => #2 date = month)
    dates)

(* Produce the number of dates in the list of dates that are in any of the months in the list of months. *)
fun number_in_months (dates, months) =
  foldl op+ 0 
    (map
      (fn (month) => number_in_month (dates, month))
      months)

(* Produce a list holding the dates from the argument list of dates that are in the month.
   The returned list should contain dates in the order they were originally given. *)
fun dates_in_month (dates: (int * int * int) list, month) =
  List.filter
    (fn (date) => #2 date = month)
    dates

(* Produce a list holding the dates from the argument list of dates that are in any of the months in the list of months. *)
fun dates_in_months (dates, months) =
  map 
    (fn (date) => #1 (valOf date)) 
    (List.filter 
      isSome
      (map
        (fn (month) => List.getItem (dates_in_month (dates, month)))
        months))

(* Produce nth element of the list where the head of the list is 1st. *)
fun get_nth (l: string list, i: int) = 
  let
    fun loop (l, i) =
      case l of
        [] => ""
      | x :: l =>
          if i = 1
            then x
          else loop (l, i - 1)
  in
    loop (l, i)
  end

(* Produce a string of the form January 20, 2013 *)
fun date_to_string ((y, m, d)) = ""

fun number_before_reaching_sum (num, l) = 0

fun what_month(day) = ceil (real (day) / (365.0 / 12.0))

fun month_range(from, to) = []

fun oldest(dates) = NONE