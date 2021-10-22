(* list of integers *)
[1; 2; 3];;
1::2::3::[];;
[1] @ [2] @ [3];;

(* function that returns prod of al elems in a list *)
(* [prod_list *)
let rec prod_list lst acc =
    match lst with
    | [] -> acc
    | h::t -> prod_list t (h * acc);;

prod_list [1; 2; 3; 4] 1;;

(* pattern matching *)

(* first elem is bigred *)
let first_br lst = 
    match lst with
    | [] -> false
    | e::_ -> e="bigred" ;;

let tlist = "bigred"::"hi"::[];;
let flist = "big"::"hi"::[];;

first_br tlist;;
first_br flist;;

(* list has 2 or 4 elems *)
let two_or_four lst = 
    match lst with 
    | [] -> false
    | [_; _] -> true
    | [_; _; _; _] -> true
    | h::t -> false;;

two_or_four [1; 2];;
two_or_four [1; 2; 3];;
two_or_four [1; 2; 3; 5];;
two_or_four [1; 2; 3; 4; 5; 6];;

(* first two elems are equal *)

let first_2_equal lst =
    match lst with
    | [] -> false
    | _::[] -> false
    | a::b::_ -> a=b;;

first_2_equal [1; 1; 3];;
first_2_equal [1; 2; 3];;


(* library *)
open List;;

let fifth_elem lst = 
    if length lst < 5 then 
        0
    else
        nth lst 4;;

fifth_elem [1; 2; 3; 4 ;5];;
fifth_elem [1; 2; 3];;

let sort_desc lst = 
    sort (fun a b -> if a = b then 0 else (if a < b then 1 else (-1))) lst;;

sort_desc [2; 4; 1];;

(* library puzzle *)

let last_elem lst = 
    nth lst ((length lst) - 1);;

last_elem [4; 5; 6; 7; 10];;

let one_zero lst =
    exists (fun a -> a=0) lst;;

one_zero [2; 3; 4];;
one_zero [0; 3; 4];;


(* take-drop *) 
(* incomplete *)
let rec take n lst acc =
    match lst with 
    | [] -> acc
    | h::t -> take (n - 1) t h::acc;;

take 1 [8; 2; 3] [];;







