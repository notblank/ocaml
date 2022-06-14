
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

(** Computing with intervals *)

(* module signature that captures the info needed about endpoints of the
   interval *)
module type Comparable = sig
    type t
    val compare : t -> t -> int
end;;

(* Functor *)

module MakeInterval (Endpoint : Comparable) = struct

    type t = | Interval of Endpoint.t * Endpoint.t
             | Empty

    (** [create low high] is the interval from [low] to [high] or is empty when
        [low > high] *)
    let create low high =
        if Endpoint.compare low high > 0 then Empty
        else Interval (low, high)

    (** true if interval is empty *)
    let is_empty = function
        |Empty -> true
        |Interval _ -> false

    (** [contains t x] is true if [x] is in [t] *)
    let contains t x =
        match t with
        |Empty -> false
        |Interval (l, h) ->
                Endpoint.compare x l >= 0 && Endpoint.compare x h <= 0

    (** [intersect t1 t2] is the intersection of interval t1 and t2 *) 
    let intersect t1 t2 =
        let min x y = if Endpoint.compare x y <= 0 then x else y in
        let max x y = if Endpoint.compare x y >= 0 then x else y in
        match t1, t2 with
        | Empty, _ | _, Empty -> Empty
        | Interval (l1, h1), Interval (l2, h2) ->
                create (max l1 l2) (min h1 h2)

end;;


