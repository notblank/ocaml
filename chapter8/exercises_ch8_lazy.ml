(** lazy hello *)

let lazy_hello =
    lazy (print_endline "Hello lazy world");;

lazy_hello;;
Lazy.force lazy_hello;;
Lazy.force lazy_hello;;

(** [p &&& q] first forces [p] if true, forces [q] and returns [p & q].*)

let ( &&& ) p q =
    if not (Lazy.force q) then false else
        let p_val = Lazy.force p in
        if p_val then p_val else false;; 

let p1 = lazy true;;
let p2 = lazy true;;
p1 &&& p2;;

