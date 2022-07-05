module type PROMISE = sig

    type 'a state = 
        |Pending
        |Resolved of 'a
        |Rejected of exn

    type 'a promise

    type 'a resolver

    (** [make ()] is a new promise and resolver. The promise is pending. *)
    val make : unit -> 'a promise *  'a resolver

    (** [return x] is a new promise resolved with value [x]. *)
    val return : 'a -> 'a promise

    (** [state p] is the state of a promise *)
    val state : 'a promise -> 'a state

    (** [resolve r x] resolves the promise [p] associated with the resolver [r]
        with value [x]. [state p] becomes [Resolved]. 
        Requires: [p] is pending. *)
    val resolve : 'a resolver -> 'a -> unit 

    (** [reject r x] rejects promise [p] associated with resolver [r] with
        exception [x]. [state p] becomes [Rejected].
        Requires: [p] is pending. *)
    val reject : 'a resolver -> exn -> unit

end;;

module Promise : PROMISE = struct

    type 'a state = 
        | Pending
        | Resolved of 'a
        | Rejected of exn

    (* reference, so it can be modified *)
    type 'a promise = 'a state ref

    type 'a resolver = 'a promise

    (** [write_once p s] changes the state of [p] to be [s]. If [p] and [s] are
        both pending, the change has no effect. 
        Raises: [Invalid_arg] if [p] is not pendng. *)
    let write_once p s =
        if !p = Pending 
        then p := s
        else invalid_arg "cannot write twice"

    let make () = 
        let p = ref Pending in (p, p)

    let return x = ref (Resolved x)

    let state p = !p

    let resolve r x = write_once r (Resolved x)

    let reject r x = write_once r (Rejected x)

    let print_promise p = 
        if !p = Resolved 
        then print_endline !p else invalid_arg "promise not resolved.";;
end;;

(* integer promise and resolver. *)

let (int_p : int Promise.promise), int_r = Promise.make ();;
Promise.state int_p;;
Promise.resolve int_r 10;;
Promise.state int_p;;

(* Promise and resolve lwt *)
#require "lwt";;
(* Lwt i/o *)
#require "lwt.unix";;
let (p : int Lwt.t), r = Lwt.wait ();;
Lwt.state p;;
Lwt.wakeup r 42;;
Lwt.state p;;

(* timing challenge 1 *)
let delay (sec : float) : unit Lwt.t = 
    Lwt_unix.sleep sec;;

(** [delay_then_print] waits for 3 seconds then prints "done".*)


