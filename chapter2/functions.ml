

(* Mutually recursive functions *)
let rec even n =
    n=0 || odd (n-1)
and odd n = 
    n<>0 && even (n-1)


(* partial application *)
let add x y = x + y;;
let add x = fun y -> x + y;;
let add = fun x -> (fun y -> x + y);;


(* debugging *)
(* problems with trace *)
let rec fib n = 
    if n <= 1 then 1 else fib(n-1) + fib(n-2);;

