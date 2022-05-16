(* Homework #1 for Coursera course Programming Languages: Part A. 
   To test this type in sml promt: - use "dates_tests.sml"; *)

(*  date is (int * int * int): year, month, day  *)
type date = int * int * int

(* Evaluates to true if the first argument is a date that comes before the
 * second argument.
 * If the two dates are the same,the result is false. *)
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
fun number_in_month (dates: date list, month) = 
  length (List.filter
    (fn (date) => #2 date = month)
    dates)

(* Produce the number of dates in the list of dates that are in any of the
 * months in the list of months. *)
fun number_in_months (dates, months) =
  foldl op+ 0 
    (map
      (fn (month) => number_in_month (dates, month))
      months)

(* Produce a list holding the dates from the argument list of dates that are in
 * the month.
 * The returned list should contain dates in the order they were originally
 * given. *)
fun dates_in_month (dates: date list, month) =
  List.filter
    (fn (date) => #2 date = month)
    dates

(* Produce a list holding the dates from the argument list of dates that are in
 * any of the months in the list of months. *)
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
    fun loop ([], i) = ""
      | loop (x :: l, i) =
          if i = 1
            then x
          else loop (l, i - 1)
  in
    loop (l, i)
  end

(* Helper function to produce nth number of month in String representation. *)
fun month_from_number (month) = get_nth (["January", "February", "March",
  "April", "May", "June", "July", "August", "September", "October", "November",
  "December"], month)

(* Produce a string of the form "January 20, 2013". *)
fun date_to_string (y, m, d) = month_from_number m ^ " " ^ (Int.toString d) ^
  ", " ^ (Int.toString y)

(* Produce an int n such that the first n elements of the list add to less than
 * sum, but the first n + 1 elements of the list add to sum or more. *)
fun number_before_reaching_sum (sum, poslist) =
  if sum <= hd poslist
    then 0
  else 1 + number_before_reaching_sum (sum - hd poslist, tl poslist)

(* Produce what month that day is in (1 for January, 2 for February, etc.) *)
fun what_month (day) =
  let 
    val days_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in 
    number_before_reaching_sum(day, days_in_months) + 1
  end

(* Produce an int list [m1, m2, ..., mn] where m1 is the month of day1, m2 is the
 * month of day1+1, ..., and mn is the month of day day2.
 * Lenght of output is day2 - day1 + 1 or 0 if day1 > day2. *)
fun month_range (day1, day2) =
  if day1 > day2
    then []
  else what_month day1 :: month_range (day1 + 1, day2)

(* Produce NONE if the list has no dates and SOME d if the date d is the oldest
 * date in the list. *)
fun oldest (dates) =
  if null dates
    then NONE
  else
    let
      val tl_ans = oldest (tl dates)
    in
      if not (isSome tl_ans) orelse is_older (hd dates, valOf tl_ans)
        then SOME (hd dates)
      else tl_ans
    end

