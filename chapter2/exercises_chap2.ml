
(* Double fun *)
let double n = 
    2 * n;;

(* write negative integers as (-2) or ~-2 *)

(* sign function *)
let sign n = 
    if n < 0 then -1 else
        if n > 0 then 1 else 0;;

(* fib and fast fib *)
let rec fib n =
    if n <= 2 then 1 else 
        fib(n-1) + fib(n-2);;


(* linear time fib *)
let rec h n pp p =
    if n = 1 then p else
        h (n-1) p (pp +p);;

let fast_fib n = h n 0 1;;

(* integer overflow fast_fib *)
fast_fib 1 lsl 1000;;


