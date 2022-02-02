(** complex synonym *)
module type ComplexSig = sig
    val zero: float * float
    val add : float * float -> float * float -> float * float
end;;


module type ComplexSig = sig
    type t 
    val zero : t
    val add : t -> t -> t
end;;

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
(** need to compare two bin trees 
 * compare depth of tree  **)

#use "bst_module.ml";;
let t = BST.Leaf;;
let t = BST.Node (2, Leaf, Leaf);;

BST.first_val t;;

module TreeKey = struct
    type t = BST.tree
    let compare t1 t2 = 
        let v0 = BST.first_val t1 in 
        let v1 = BST.first_val t2 in 
        BST.max_t t1 v0 - BST.max_t t2 v1
end;;


(** Data struct to map tree to anything *)
module TreeMap = Map.Make(TreeKey);;

(** trees to lists *)
let m = 
    let open TreeMap in
    empty
    |> add Leaf [] 
    |> add (Node (2, Leaf, Leaf)) [2];;

(* functions of the Map module *)
TreeMap.mem Leaf m;;
TreeMap.find Leaf m;;
let t = BST.Node (2, Leaf, Leaf);;
TreeMap.mem t m;;
