
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
Hashtbl.find tab "21";;
Hashtbl.find tab "100";;

(* hashtbl stats *)
Hashtbl.stats tab;;

(* hashtbl bindings *)
(* in progress: no clue *)
Hashtbl.fold (fun a b init -> b + init) tab 0;;

(* hashtbl load factor *)

let load_factor ht = 
    let ht_stats = Hashtbl.stats ht in
    (float_of_int ht_stats.num_bindings)/.(float_of_int ht_stats.num_buckets);;

(* resize tab when load factor > 2 *)
load_factor tab;;
Hashtbl.add tab "32" 32;;
load_factor tab;;
Hashtbl.add tab "33" 33;;
load_factor tab;;

(* functorial interface *)
(* in progress: Study Make functor *)
let ci_equal s1 s2 =
    let ls1 = (String.lowercase_ascii s1) in
    let ls2 = (String.lowercase_ascii s2) in
    String.equal ls1 ls2

(* if two keys are equal according to equal, then they should hash to the same
   value *)
module CiStrHash = 
    struct
        type t = string
        let equal s1 s2 = ci_equal s1 s2
        let hash s = Hashtbl.hash (String.lowercase_ascii s)
    end;;

module CiStrHashtbl = Hashtbl.Make(CiStrHash);;

let h = CiStrHashtbl.create 11;;
CiStrHashtbl.add h "Hola" 21;;
CiStrHashtbl.find h "Hola";;
CiStrHashtbl.find h "hola";;
CiStrHashtbl.find h "hoLa";;

(* bad hash *)
(* above set hash to a constant and use stats *)

(* linear probing *)


