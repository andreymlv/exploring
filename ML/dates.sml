(* Homework #1 for Coursera course Programming Languages: Part A *)
fun is_older((year_1,month_1,day_1),(year_2,month_2,day_2)) = 
    if year_1 < year_2 
    then true 
    else if month_1 < month_2 
    then true 
    else if day_1 < day_2 
    then true 
    else false
