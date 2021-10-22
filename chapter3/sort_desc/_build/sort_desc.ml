open List

let sort_desc lst = 
    sort (fun a b -> if a = b then 0 else (if a < b then 1 else (-1))) lst
