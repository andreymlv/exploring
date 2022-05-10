(* Homework #1 for Coursera course Programming Languages: Part A. *)

(* Evaluates to true if the first argument is a date that comes before the second argument.
   If the two dates are the same,the result is false. *)
fun is_older((year_1, month_1, day_1), (year_2, month_2, day_2)) =
  if year_1 < year_2
  then true
  else if month_1 < month_2
  then true
  else if day_1 < day_2
  then true
  else false

(* Produce how many dates in the list are in the given month. *)
fun number_in_month(dates : (int * int * int) list, month) = 
  List.length(List.filter (fn (date) => #2 date = month) dates)

(* Produce the number of dates in the list of dates that are in any of the months in the list of months. *)
fun number_in_months(_, []) = 0
  | number_in_months(dates, months) = number_in_month(dates, hd months) 
                                    + number_in_months(dates, tl months)

(* Produce a list holding the dates from the argument list of dates that are in the month.
   The returned list should contain dates in the order they were originally given. *)
fun dates_in_month(dates : (int * int * int) list, month) =
  List.filter (fn (date) => #2 date = month) dates
