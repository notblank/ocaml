(** [Poly] represents immutable polynomials with integer coefficients. *)
module type PolySig = sig
  (** [t] is the type of polynomials *)
  type t

  (** [eval x p] is [p] evaluated at [x]. Example: if [p] represents
      $3x^3 + x^2 + x$, then [eval 10 p] is [3110]. *)
  val eval : int -> t -> int

  (** [sum p q] produces poly [p + q]. Example: if [p] is $2x + 4$ and [q] is
   * $3x^2 + x - 2$, then [p + q] is $3x^2 + 3x + 2$.*)
  val sum : t -> t -> t

end


(** implementation as lists of coefficients where $a_i$ is in position [i].*)

let rec sum lst = 
    match lst with
    | [] -> 0
    | h::t -> h + sum t

(** [pow x n] is $x^n$. *)
let rec pow x n = 
    match n with 
    | 0 -> 1
    | n -> x * (pow x (n - 1))

(** [x_n x lst] is the list of monomials $x^i$ for the indices of [lst]. *)
let x_n x = List.mapi (fun i a -> pow x i)

(** [a_x a x] is the list of $a_i x_i$ products.*)
let a_x = (List.map2 (fun a x -> a * x))

let rec pad_rev a b_rev p = 
    let n0 = List.length a - List.length b_rev in
    match n0 with 
    | 0 -> b_rev
    | n -> pad_rev a (p::b_rev) p;;

(** [pad a b] Requires: length a >= length b. Pads [b] with $0$ so that length
 * of [b_pad] is equal to length of [a] *)
let pad a b = List.rev(pad_rev a (List.rev b) 0);;

module Poly : PolySig = struct
    (** Coefficient $a_i$ of a polynomial is contained in postion [i] of a list
     * *)
    type t = int list 

    let eval x p = 
        let xn  = pow_x x p in sum (a_x p xn)

    let sum p q = 
        if List.compare_lengths p q = 0 then List.map2 ( + ) p q
        else 
            if List.compare_lengths p q = 1 then 
                List.map2 ( + ) p (pad p q)
                else List.map2 ( + ) (pad q p) q

end

(** [get_index lst] is the list of indices of lst. *)
let get_index = List.mapi (fun i a -> i);;

(** [sum_lists a b] is the list with entries [ai + bi] *)
let sum_lists = List.map2 (fun a b -> a + b);;

(** *)
let rec zeroes n l =
    if n = 0 then l 
    else zeroes (n - 1) (0:: l);;

zeroes 10 [];;



(** idea multiply and keep track of indices to form pq monomials *)
sum_lists [2; 3; 4] [1; 1; 1];;
get_index [2; 3; 4];;



