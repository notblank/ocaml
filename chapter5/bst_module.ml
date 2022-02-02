module BST = struct

    type tree = 
        | Leaf
        | Node of int * tree * tree;;

    (* 0 is matched with the empty tree *)
    let first_val t = 
        match t with 
        |Leaf -> 0 
        |Node (v, l, r) -> v;;

    (* min and max of a tree *)
    let rec fun_t_aux t v0 f = 
        match t with
        | Leaf -> v0 
        | Node (v, l, r) -> 
                let mt = (f (fun_t_aux l v0 f)(fun_t_aux r v0 f)) in f v mt;; 

    let max_t t v0 = 
        fun_t_aux t v0 max;;

    let min_t t v0 = 
        fun_t_aux t v0 min;;

    (* is bst *)
    let rec is_bst t = 
        match t with
        | Leaf -> true
        | Node (v, l, r) -> 
                let vr = min_t r v in
                let vl = max_t l v in
                    v >= vl && v <= vr && is_bst l;;

end
