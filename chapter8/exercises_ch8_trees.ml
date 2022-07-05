(* functorized BST *)

module type Comparable = sig
    type t
    val compare : t -> t -> int
end;;

module BSTSet (BST : Comparable) = struct

    type 'a tree = Node of 'a * 'a tree * 'a tree
                  |Leaf

    (** [mem x t] is true uf [x] is in [t]. *)
    let rec mem x = function
        | Leaf -> false
        | Node (v, l, r) ->
                if BST.compare x v < 0 then mem x l else 
                if BST.compare x v > 0 then mem x r else 
                    true
    
    (* [insert x t] inserts [x] into [t].*)
    let rec insert x t = 
        match t with 
        | Leaf -> Node (x, Leaf, Leaf)
        | Node (v, l, r) -> 
                if BST.compare x v < 0 then mem x l else 
                if BST.compare x v > 0 then mem x r else 
                    t

end;;

