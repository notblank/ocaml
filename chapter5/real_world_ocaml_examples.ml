
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

(* Can apply to a module with the right structure *)
module IntInterval =
    MakeInterval(
        struct
            type t = int
            let compare = Int.compare
        end);;

(* Apply directly if functor is aligned with standard modules *)
module IntInterval = MakeInterval(Int);;

let i1 = IntInterval.create 3 5;;

(* Representation invariant of lower < upper can be violated *) 
let i2 = IntInterval.Interval (4,3);;
IntInterval.is_empty i2;;

(* Make the type Interval abstract so users cannot use it to violate the RI by
   restricting the output of MakeInterval *)

module type IntervalOut = sig
    type t
    type endpoint
    val create : endpoint -> endpoint -> t
    val is_empty : t -> bool
    val contains : t -> endpoint -> bool
    val intersect : t -> t -> t
end;;

module MakeIntervalAbs (Endpoint : Comparable) : IntervalOut = struct

    type endpoint = Endpoint.t
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

module IntIntervalAbs = MakeIntervalAbs(Int);;
IntIntervalAbs.Interval (5,3);;
(* too abstract: need to expose the type of endpoint *)
IntIntervalAbs.create 3 4;;

(* to expose that enpoint is of type Endpoint.t can share constraints *)

(* in the signature, by writing:*)
module type IntIntervalExp = 
    IntervalF with type endpoint = int;;

(* in the functor, by writing: *)
module MakeIntervalExp (Endpoint : Comparable) :
    (IntervalOut with type endpoint = Endpoint.t) 
    = struct
        type endpoint = Endpoint.t
        type t = |Interval of Endpoint.t * Endpoint.t
                 |Empty

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


module IntIntervalExp = MakeIntervalExp(Int);;
module IntIntervalAbs = MakeIntervalAbs(Int);;

IntIntervalExp.create 3 5;;
IntIntervalAbs.create 3 5;;

