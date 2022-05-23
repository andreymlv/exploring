(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string (s1: string, s2: string): bool =
  s1 = s2

(* put your solutions for problem 1 here *)

(* Return NONE if the string is not in the list, 
 * else return SOME lst where lst is identical to the argument list except the
 * string is not in it. *)
fun all_except_option (s: string, lst: string list): string list option =
  case lst of
    [] => NONE
  | x :: xs =>
    let
      val other = all_except_option (s, xs)
      val is_same = same_string (s, x)
    in
      case other of
        NONE   => if is_same then SOME (xs) else NONE
      | SOME y => if is_same then other else SOME (x :: y)
    end

(* The result has all the strings that are in some list in substitutions that also has s,
 * but s itself should not be in the result. *)
fun get_substitutions1 (lst: string list list, s: string): string list =
  case lst of
     [] => []
    | x :: xs =>
      case all_except_option (s, x) of
        NONE   => get_substitutions1 (xs, s)
      | SOME y => y @ get_substitutions1 (xs, s)

(*  returns list of other strings from lists that contain given string
 *  assumes each list has no repetitions, is tail recursive  *)
fun get_substitutions2(lst: string list list, s: string): string list =
    let
        fun aux(lst: string list list, s: string, acc: string list): string list =
            case lst of
              [] => acc
            | x :: xs =>
                case all_except_option(s, x) of
                  NONE   => aux(xs, s, acc)
                | SOME y => aux(xs, s, acc @ y)
    in
        aux(lst, s, [])
    end

(*
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
*)