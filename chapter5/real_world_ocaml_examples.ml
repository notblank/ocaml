
module type X = sig
    val x : int
end;;

(* functor *)
(* anonymous functor/function syntax. *)
(* need to specify the input type. *)
module IncX (M : X) = struct
    let x = M.x + 1
end;;

(* ocaml cornell:
module IncX = functor (M : X) -> struct
    let x = M.x + 1
end;;
*)

(* Using the functor *)

module Three = struct
    let x = 3
end;;

(* cannot write IncX(Three) directly in utop. 
   Use module definition to execute.*)
module Four = IncX(Three);;
Four.x;;
