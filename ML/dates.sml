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

(* Produce how many dates in the list are in the given month number. *)
fun number_in_month(months : (int * int * int) list, number) =
   if null months
   then 0
   else if #2(hd months) = number
   then 1 + number_in_month(tl months, number)
   else number_in_month(tl months, number)

(* Produce the number of dates in the list of dates that are in any of the months in the list of months (numbers). *)
fun number_in_months(months, numbers) =
   if null numbers
   then 0
   else number_in_month(months, hd numbers) 
      + number_in_months(months, tl numbers)
