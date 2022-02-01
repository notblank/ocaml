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
let rec drop n lst =
    match n, lst with 
    | _, [] -> []
    | 0, lst -> lst
    | n, h::t -> drop (n - 1) t;;

drop 2 [1; 2; 3; 5];;

let take n lst =
    let n_drop = (length lst) - n in
    rev (drop n_drop (rev lst));;


let rec from i j l = if i > j then l else from i (j - 1) (j :: l);;

(* long lists to test tail-recursion of drop and take *)
(** [i -- j] is the list containing the integers from [i] to [j], inclusive. *)
let ( -- ) i j = from i j [];;

let long_list = 0 -- 1_000_000;;

take 10000 long_list;;


(* BTS condition *)
(* For any node n, every node in the left subtree of n has a value less than
 * n’s value, and every node in the right subtree of n has a value greater than
 * n’s value. *)
(* write a function that verifies if a tree is a BST *)

(* let is_bst = *)

type 'a tree = 
    | Leaf
    | Node of 'a * 'a tree * 'a tree;;


let first_val t = 
    match t with 
    |Node (v, l, r) -> v;;

(* min and max of a tree *)
let rec fun_t_aux t v0 f = 
    match t with
    | Leaf -> v0 
    | Node (v, l, r) -> 
            let mt = (f (fun_t_aux l v0 f)(fun_t_aux r v0 f)) in f v mt;; 

let max_t t = 
    let v0 = first_val t in fun_t_aux t v0 max;;

let min_t t = 
    let v0 = first_val t in fun_t_aux t v0 min;;

(* in progress *)
let rec is_bst t = 
    match t with
    | Leaf -> true
    | Node (v, l, r) -> 
            let vl = min_t r in
            let vr = max_t l in
                v >= vl && v <= vr && is_bst l;;
(* examples *)
let t = 
    Node(-15, 
        Node(-20, Leaf, Leaf),
        Node(-3, 
            Node(-10, Leaf, Leaf), 
            Leaf)
        );;

let t = 
    Node(2, 
        Node(1, Leaf, Leaf),
        Node(5, 
            Node(4, Leaf, Leaf), 
            Node(18, Leaf, Leaf))
        );;

is_bst t;;


let maxt = max_t t;;
let mint = min_t t;;


