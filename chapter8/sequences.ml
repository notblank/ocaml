type 'a sequence = Cons of 'a * (unit -> 'a sequence);;

let rec from n = Cons (n, fun () -> from (n + 1));;

let nats = from 0;;


(* Some functions on sequences *)

let hd (Cons (h, t)) = h;;
(* [t : unit -> 'a sequence] is applied to [unit] *)
let tl (Cons (_, t)) = t ();;

(* starts at 1 *)
tl nats;;

let rec square (Cons (h, t)) = Cons (h * h, fun () -> square (t ()));;
let rec sum (Cons (h1, t1)) (Cons (h2, t2)) =
    Cons (h1 + h2, fun () -> sum (t1 ()) (t2 ()));;

(* Laziness *)

let rec take n s =
    if n = 0 then [] else hd s :: take (n - 1) (tl s);;

let rec fibs = 
    Cons (1, fun () ->
        Cons (1, fun () ->
            sum fibs (tl fibs)));;

take 10 fibs;;

let fib30_lazy = lazy (take 30 fibs |> List.rev |> List.hd);;
let fib30 = Lazy.force fib30_lazy;;
fib30;;


(* Lazy sequences *)
(* use lazy val for the tail *)

type 'a lazy_seq = Cons of 'a * 'a lazy_seq Lazy.t;;
