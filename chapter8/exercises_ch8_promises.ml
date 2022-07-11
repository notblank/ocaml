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

    (** [bind p c] binds the callback function [c] to the promise [p]. If [p]
        is resolved run [c] on the contents of [p]. If [p] is rejected don't
        run [c]. If [p] is pending return the promise of running [c].*)
    val (>>=) : 'a promise -> ('a -> 'b promise) -> 'b promise

end;;

module Promise : PROMISE = struct
  type 'a state = Pending | Resolved of 'a | Rejected of exn

  (** RI: the input may not be [Pending] *)
  type 'a handler = 'a state -> unit

  (** RI: if [state <> Pending] then [handlers = []]. *)
  type 'a promise = {
    mutable state : 'a state;
    mutable handlers : 'a handler list
  }

  let enqueue
      (handler : 'a state -> unit)
      (promise : 'a promise) : unit
    =
    promise.handlers <- handler :: promise.handlers

  type 'a resolver = 'a promise

  (** [write_once p s] changes the state of [p] to be [s].  If [p] and [s]
      are both pending, that has no effect.
      Raises: [Invalid_arg] if the state of [p] is not pending. *)
  let write_once p s =
    if p.state = Pending
    then p.state <- s
    else invalid_arg "cannot write twice"

  let make () =
    let p = {state = Pending; handlers = []} in
    p, p

  let return x =
    {state = Resolved x; handlers = []}

  let state p = p.state

  (** requires: [st] may not be [Pending] *)
  let resolve_or_reject (r : 'a resolver) (st : 'a state) =
    assert (st <> Pending);
    let handlers = r.handlers in
    r.handlers <- [];
    write_once r st;
    List.iter (fun f -> f st) handlers

  let reject r x =
    resolve_or_reject r (Rejected x)

  let resolve r x =
    resolve_or_reject r (Resolved x)

  let handler (resolver : 'a resolver) : 'a handler
    = function
      | Pending -> failwith "handler RI violated"
      | Rejected exc -> reject resolver exc
      | Resolved x -> resolve resolver x

  let handler_of_callback
      (callback : 'a -> 'b promise)
      (resolver : 'b resolver) : 'a handler
    = function
      | Pending -> failwith "handler RI violated"
      | Rejected exc -> reject resolver exc
      | Resolved x ->
        let promise = callback x in
        match promise.state with
        | Resolved y -> resolve resolver y
        | Rejected exc -> reject resolver exc
        | Pending -> enqueue (handler resolver) promise

  let (>>=)
      (input_promise : 'a promise)
      (callback : 'a -> 'b promise) : 'b promise
    =
    match input_promise.state with
    | Resolved x -> callback x
    | Rejected exc -> {state = Rejected exc; handlers = []}
    | Pending ->
      let output_promise, output_resolver = make () in
      enqueue (handler_of_callback callback output_resolver) input_promise;
      output_promise

end;;

(* integer promise and resolver. *)
(* in progress: print after resolved:
    idea: create a print callback and bind.
    problem: don't know how to return a promise. *)

let (int_p : int Promise.promise), int_r = Promise.make ();;
Promise.state int_p;;

let print_int_promise (p : int Promise.promise)  = 
    let out_p, out_r = Promise.make () in 
    let st = Promise.state p in
    match st with
    |Resolved x -> 
            let out_st = Promise.state out_p in
            out_st <- Resolved (print_int x);;

Promise.resolve int_r 10;;
Promise.state int_p;;

(* Promise and resolve lwt *)
#require "lwt";;
(* Lwt i/o *)
#require "lwt.unix";;
let (p : int Lwt.t), r = Lwt.wait ();;
let c_fnt int_p = Lwt_io.printf "The value is: %i\n" int_p;;
let c_prom = read_line Lwt_io.stdin in Lwt.bind p c_fnt;;

Lwt.state p;;
Lwt.state c_prom;;

Lwt.wakeup r 42;;
Lwt.state p;;
Lwt.state c_prom;;

(* timing challenge 1 *)
let delay (sec : float) : unit Lwt.t = 
    Lwt_unix.sleep sec;;

(** [delay_then_print] waits for 3 seconds then prints "done".*)



