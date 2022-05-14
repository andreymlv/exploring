(* Homework1 Simple Test *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)
use "dates.sml";

val test1_older = is_older ((1, 2, 3), (2, 3, 4)) = true
val test2_older = is_older ((2, 2, 2), (2, 2, 2)) = false
val test3_older = is_older ((2, 3, 4), (1, 2, 3)) = false

val test1_number_in_month = number_in_month ([(2012, 2, 28), (2013, 12, 1)], 2) = 1
val test2_number_in_month = number_in_month ([(2012, 2, 28), (2013, 2, 1)], 2) = 2
val test3_number_in_month = number_in_month ([(2012, 2, 28), (2013, 12, 1)], 3) = 0

val test1_number_in_months = number_in_months ([(2012, 2, 28), (2013, 12, 1),
  (2011, 3, 31), (2011, 4, 28)], [2, 3, 4]) = 3
val test2_number_in_months = number_in_months ([(2012, 2, 28), (2013, 12, 1),
  (2011, 3, 31), (2011, 4, 28)], [5, 6, 7]) = 0

val test1_dates_in_month = dates_in_month ([(2012, 2, 28), (2013, 12, 1)], 2) =
  [(2012, 2, 28)]
val test2_dates_in_month = dates_in_month ([(2012, 2, 28), (2013, 2, 1)], 2) =
  [(2012, 2, 28), (2013, 2, 1)]
val test3_dates_in_month = dates_in_month ([(2012, 2, 28), (2013, 12, 1)], 4) =
  []

val test1_dates_in_months = dates_in_months ([(2012, 2, 28), (2013, 12, 1),
  (2011, 3, 31),(2011, 4, 28)],[2, 3, 4]) = [(2012, 2, 28), (2011, 3, 31),
    (2011, 4, 28)]
val test2_dates_in_months = dates_in_months ([(2012, 2, 28), (2013, 12, 1),
  (2011, 3, 31),(2011, 4, 28)],[]) = []
val test3_dates_in_months = dates_in_months ([(2012, 2, 28), (2013, 12, 1),
  (2011, 3, 31),(2011, 4, 28)],[6, 7, 8]) = []

val test1_get_nth = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"
val test2_get_nth = get_nth ([], 2) = ""
val test3_get_nth = get_nth (["hi", "there", "how", "are", "you"], 1) = "hi"
val test4_get_nth = get_nth (["hi", "there", "how", "are", "you"], 4) = "are"

val test1_date_to_string = date_to_string (2013, 6, 1) = "June 1, 2013"
val test2_date_to_string = date_to_string (2022, 12, 28) = "December 28, 2022"

val test1_number_before_reaching_sum = number_before_reaching_sum (10, [1, 2, 3,
  4, 5]) = 3
val test2_number_before_reaching_sum = number_before_reaching_sum (10, [4, 3, 2,
  1, 5]) = 2

val test1_what_month = what_month 70 = 3
val test2_what_month = what_month 128 = 5

val test1_month_range = month_range (31, 34) = [1, 2, 2, 2]

val test1_oldest = oldest ([(2012, 2, 28), (2011, 3, 31), (2011, 4, 28)]) = SOME
  (2011, 3, 31)
