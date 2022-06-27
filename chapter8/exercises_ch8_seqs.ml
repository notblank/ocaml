type 'a sequence = Cons of 'a * (unit -> 'a sequence);;

let rec from n = Cons (n, fun () -> from (n + 1));;
let nats = from 0;;

(* head and tail of a sequence *)
let hd (Cons (h, _)) = h;;
let tl (Cons (_, tl)) = tl ();;

let rec take n s =
    if n = 0 then [] else hd s :: take (n - 1) (tl s);;

(* pow2 *)
let rec pow2 (Cons (h, t)) = Cons (2*h, fun () -> pow2 (t ()));;
let pow2_nats = pow2 nats;;

take 10 pow2_nats;;

(* more sequences *)
(* even naturals *)
let rec skip_1 n = Cons (n, fun () -> pairs (n + 2));;
let even_nats = skip_1 0;;
take 100 even_nats;;

(* lowercase alphabet on repeat *)
let rec letters_from n = 
    Cons (Char.chr ((n mod 25) + 65), fun () -> letters_from (n + 1));;

let letters = letters_from 0;;
take 100 letters;;

(* unending coinflips *)
let rec flips_from n = 
    Cons (Random.int 2, fun () -> flips_from (n + 1));;

let flips = flips_from 0;;
take 100 flips;;

(* nth *)
let rec nth s n =
    if n = 0 then hd s else nth (tl s) (n - 1);;

nth letters 105;;

(* hd tl *)
(* explain how these are evaluated *)

hd nats;;
tl nats;;
hd (tl nats);;
tl (tl nats);;
hd (tl (tl nats));;

(* filter *)
(** [filter p s] is the subsequence of s that satisfies predicate p *)

let rec filter p (Cons (h, t)) = 
    if p h then Cons (h, fun () -> filter p (t ())) else filter p (t ());;

let pairs = filter (fun n -> (n mod 2) = 0) nats;;
take 100 pairs;;

(** [interleave s t] is the sequence <s1; t1; s2; t2; ... > *)
let rec interleave (Cons (h1, t1)) (Cons (h2, t2)) =
    Cons (h1, fun () -> interleave (Cons (h2, t2)) (t1 ()));;

let odds = filter (fun n -> (n mod 2) = 1) nats;;

take 100 (interleave pairs odds);;

(** [sift] are the prime numbers computed using the Sieve of Eratosthenes. *)

let rec sift nums =
    let fnums = filter (fun n -> (n mod (hd nums)) <> 0) nums in
    Cons (hd nums, fun () -> sift fnums);;

(* primes *)
let primes = sift (from 2);;
take 100 primes;;

(** [e_terms x] contains the terms of the expansion for [e^x]. *)

let fact n =
  let rec fact_aux n acc =
    if n = 0 then acc else fact_aux (n - 1) (n * acc) in
  fact_aux n 1;;

let rec e_terms0 x (Cons (h, t)) k = 
    let fact_k = float_of_int (fact k) in
    let pow_xh = Float.pow x (float_of_int h) in
    Cons (pow_xh /. fact_k, fun () -> e_terms0 x (t ()) (k + 1));;

let e_terms x = e_terms0 x (from 0) 0;;
take 10 (e_terms 1.0);;

(** [total s] is the sequence of sums [s1; s1 +. s2; s1 +. s2 +. s3; ... ]. *)
(* in progress *)
let rec total (Cons (h, t)) =
    let hd_t = hd (t ()) in
    let sum_s = Cons (h + hd_t, t) in
    Cons (h + hd_t, fun () -> total sum_s);;

take 10 (total (from 1));;
take 10 (from 1);;




