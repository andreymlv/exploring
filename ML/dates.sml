(* Homework #1 for Coursera course Programming Languages: Part A. *)

(* Evaluates to true if the first argument is a date that comes before the second argument.
   If the two dates are the same,the result is false. *)
fun is_older((y1, m1, d1), (y2, m2, d2)) =
  let
    fun cmp (v1, v2, cmpnext) = 
      case Int.compare (v1, v2) of
        EQUAL => cmpnext ()
      | r => r
  in
    cmp(y1, y2,
    fn _ => cmp(m1, m2,
    fn _ => cmp(d1, d2,
    fn _ => EQUAL))) = LESS
  end

(* Produce how many dates in the list are in the given month. *)
fun number_in_month(dates : (int * int * int) list, month) = 
  length (List.filter (fn (date) => #2 date = month) dates)

(* Produce the number of dates in the list of dates that are in any of the months in the list of months. *)
fun number_in_months(dates, months) =
  foldl op+ 0 (map (fn (month) => number_in_month(dates, month)) months)

(* Produce a list holding the dates from the argument list of dates that are in the month.
   The returned list should contain dates in the order they were originally given. *)
fun dates_in_month(dates : (int * int * int) list, month) =
  List.filter (fn (date) => #2 date = month) dates
