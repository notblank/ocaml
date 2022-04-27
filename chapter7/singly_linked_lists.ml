(** single linked list *)
type 'a node = {
    mutable next : 'a node option;
    value : 'a
};;

type 'a mlist = {
    mutable first : 'a node option
};;

(** operations *)
let insert_first lst v = 
    lst.first <- Some {next = lst.first; value = v};;

(** [empty ()] is the empty mlist *)
let empty () = {first = Some {next = None; value = None}};;

let rec map_f_nodes first_node f =
    match first_node with 
    | None -> None 
    | Some {value; next} -> Some {next = map_f_nodes next f; value = f value};;

(** [map_f lst f] is a mlist with node values [f v]. Maps f forward. *)
let map_f lst f = 
    lst.first <- map_f_nodes lst.first f;;

let rec insert_after_node0 fnode n v =
    let fnode0 = Option.get fnode in
    if fnode0 = n then
        Some {next = Some n; value = v}
    else
        Some {next = insert_after_node0 fnode0.next n v; value = fnode0.value};;

(** [before_node lst n v] is an mlist with value v inserted after node n*)
let after_node lst n v =
    lst.first <- insert_after_node0 lst.first n v;;
(** todo: remove node *) 

(** examples *)
let singleton v = {
    first = Some {next = None; value = v}
};;

let create_node v = {
    next = None;
    value = v
};;

let n = create_node 3;;
let l = singleton 1;;
insert_first l 5;;

insert_after l nn 8;;

let nn = {next = None; value = 1};;
insert_before_node l.first nn 9;;


