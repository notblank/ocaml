
module type X_int = sig
    val x : int
end;;

(* functor *)
module Increment (M : X_int) : X_int = struct
    let x = M.x + 1
end;;

(* Using the functor *)

module Three = struct
    let x = 3
end;;

module Four = Increment(Three);;
Four.x;;
