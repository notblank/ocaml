(** complex synonym *)
module type ComplexSig = sig
    val zero: float * float
    val add : float * float -> float * float -> float * float
end;;


module type ComplexSigImp = sig
    type t 
    val zero : t
    val add : t -> t -> t
end

(** encapsulation *)
(** remove zero then add from signature. change zero to 0., 0. *)
(** removing something from sig hides it in the implementation *)
module Complex : ComplexSig = struct
    type t = float * float
    let zero = 0., 0.
    let add (r1, i1) (r2, i2) = (r1 +. r2, i1 +. i2)
end;;

(** binary search tree map *)
(** write module BstMap using Map *)

type 'a tree = Node of 'a * 'a tree * 'a tree | Leaf;;


(** need to compare two bin trees 
 * compare depth of tree
module TreeMap = struct
    type t = tree
    let compare t1 t2 = 
*)


