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
    | Some {value; next} -> Some {next = map_f next f; value = f value};;

(** [map_f lst f] is a mlist with node values [f v].*)
let map_f lst f = 
    lst.first <- map_f_nodes lst.first f;;

(** in progress *)
let rec insert_before_node first_node n v =
    match first_node with
    | n -> Some {next = n.next; value = v}
    | Some {value; next} -> 
            Some {next = insert_before_node next n v; value = value};; 


(** [to_list lst] is an OCaml list containing the same values as [lst]
    in the same order. Not tail recursive. *)
let rec to_list (lst : 'a mlist) : 'a list =
  match !lst with None -> [] | Some { next; value } -> value :: to_list next

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
insert_after_first l 7;;

map_f l (fun x -> x*x);;

insert_after l nn 8;;




