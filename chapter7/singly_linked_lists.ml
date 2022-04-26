(** single linked list *)
type 'a node = {
    mutable next : 'a node option;
    value : 'a
};;

type 'a mlist = {
    mutable first : 'a node option
};;

let create_node v = {
    next = None;
    value = v
};;

(** operations *)
let insert_first lst v = 
    lst.first <- Some {next = lst.first; value = v};;

let pop lst = 
    match lst.first with
    |None -> {first = None}
    |Some v -> {first = v.next};;

(** Syntax error: then and else expression should have same type
let rec insert_after lst n v =
    if lst.first = n then 
        lst.first <- Some {next = n.next; value = v}
    else
        insert_after (pop lst) n v;;
*)

let empty () = {first = Some {next = None; value = None}};;


(** examples *)
let singleton v = {
    first = Some {next = None; value = v}
};;

let l = singleton 1;;
insert_first l 3;;
l;;

