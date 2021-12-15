
let double x = 2*x;;
let square x = x*x;;
let twice f x = f (f x);;
let quad = twice double;;
(*  val quad : int -> int = <fun> has type function of int because twice takes
 *  in a function of type a' -> a' and an argument of type a' so twice double
 *  is a function that takes an argument of type int and returns an int *)
let fourth = twice square;;

(* what does $ do? *)
let ( $ ) f x = f x;;

(* this evaluates the function on the right first and then applies square *)
square $ 2 + 2;; 
square $ (+) 2 2;;
(* this means (square 2) + 2) *)
square 2 + 2;; 


(* repeat function *)
(* not tail recursive *)
let rec repeat f n x = 
    match n with 
    | 0 -> x
    | n -> f (repeat f (n - 1) x);;


let times_eight = repeat double 3;;
times_eight 2;;    

(* prod left and right *)
let prod_left = List.fold_left ( * ) 1;;

prod_left [2; 2; 3];;

(* sum cube odd *)

let rec ( --- ) i j = if i > j then [] else i :: i + 2 -- j;;
let cube x = x * x * x;;
let sum = List.fold_left ( + ) 0;;


let sum_cube_odd n =
  0 --- n              
  |> List.map cube  
  |> sum;;

sum_cube_odd 5;;

(* account balance *)
let balance_left bal = List.fold_left ( - ) bal;;
(* right in progress *)
let balance_right lst bal = List.fold_right ( - ) lst bal;;

balance_left 5 [1; 2; 1];;
balance_right [1; 2; 1] 4;;

(* map f (map g lst) with one map*)
 
let compose f g x = x |> g |> f;;
let compose_list f g lst = List.map (compose f g) lst;;

compose_list square double [2; 3; 1];;
