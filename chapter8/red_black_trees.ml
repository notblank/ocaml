module RBTree = struct

    type color = Red | Black
    type 'a rbtree = Leaf 
        | Node of color * 'a * 'a rbtree * 'a rbtree
    
    (* Okasaki *)
    let balance = function
      | Black, z, Node (Red, y, Node (Red, x, a, b), c), d
      | Black, z, Node (Red, x, a, Node (Red, y, b, c)), d
      | Black, x, a, Node (Red, z, Node (Red, y, b, c), d)
      | Black, x, a, Node (Red, y, b, Node (Red, z, c, d)) ->
        Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
      | a, b, c, d -> Node (a, b, c, d)

    let rec insert_aux x = function 
        | Leaf -> Node (Red, x, Leaf, Leaf)
        | Node (color, y, a, b) as s ->
                    if x < y then balance (color, y, insert_aux a, b) 
                    else
                        if x > y then balance (color, y, a, insert_aux b) 
                    else s

    let insert x s =
        match insert_aux x s with
        | Node (_, y, a, b) -> Node (Black, y, a, b)
        | Leaf -> faliwith "RBT insert failed with ins returning leaf"

end;;
