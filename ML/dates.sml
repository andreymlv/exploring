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
   if null dates
   then 0
   else if #2(hd dates) = month
   then 1 + number_in_month(tl dates, month)
   else number_in_month(tl dates, month)

(* Produce the number of dates in the list of dates that are in any of the months in the list of months. *)
fun number_in_months(dates, months) =
   if null months 
   then 0
   else number_in_month(dates, hd months) 
      + number_in_months(dates, tl months)

(* Produce a list holding the dates from the argument list of dates that are in the month.
   The returned list should contain dates in the order they were originally given. *)
fun dates_in_month(dates : (int * int * int) list, month) =
   if null dates
   then nil
   else if #2(hd dates) = month
   then hd dates :: dates_in_month(tl dates, month)
   else dates_in_month(tl dates, month)
