(** mutable fields *)

type gpa = {name : string; mutable gpa : float};;

let pedro = {name = "Pedro"; gpa = 3.0};; 
pedro.gpa;;
pedro.gpa <- 2.5;;
pedro;;

(** give ocaml expressions with the following types *)

(** bool ref *)
ref true;;

(** inc fun *)
(** reference to a function *)
let inc_0 = ref (fun x -> x + 1);;
(** produce 3110 using inc *)
!inc 1;;

let inc n = if n = 0 then 0 else 1 + !inc_0 (n - 1);;
inc 3109;;

(** addition assigment *)
(** a += b is a = a + b *)

let (+=) x y = 
    x := !x + y;;

let x = ref 3;;
x += 2;;
x;;

(** doubly linked list *)

type 'a node = {
    mutable prev : 'a node option;
    mutable next : 'a node option;
    value : 'a
};;

type 'a dlist = {
    mutable first : 'a node option;
    mutable last : 'a node option
};;

let create_node v = {
    prev = None;
    next = None;
    value = v
};;

let singleton v = {
    first = Some (create_node v);
    last = None 
};;


(** operations *)
let insert_first_d lst v =
    lst.first <- Some {prev = None; next = lst.first; value = v};;

let empty_d () = 
    Some {first = None; last = None};;

let insert_after n v =


let d_list = singleton 2;;
insert_first_d d_list 5;;
d_list;;


