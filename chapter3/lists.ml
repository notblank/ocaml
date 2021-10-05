
(* valid for int list only *)
let rec sum lst =
    match lst with
    | [] -> 0
    | h::t -> h + sum t;;
