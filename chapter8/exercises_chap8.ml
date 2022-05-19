
(* hash insert *)



(* hash values *)

Hashtbl.hash;;

Hashtbl.hash ();;
Hashtbl.hash false;;
Hashtbl.hash true;;
Hashtbl.hash 0;;
Hashtbl.hash 1;;
Hashtbl.hash [];;

(* hashtbl usage *)

let rec from i j l = if i > j then l else from i (j - 1) (j :: l);;
let ( -- ) i j = from i j [];;

let nums = 1 -- 31;;
let nums_keys = List.map string_of_int nums;;

let tab = Hashtbl.create 16;;
List.map2 (Hashtbl.add tab) nums_keys nums;;

List.map (Hashtbl.find tab) nums_keys;;
Hashtbl.find tab "100";;

(* hashtbl stats *)
Hashtbl.stats tab;;

(* hashtbl bindings *)


