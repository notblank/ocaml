type 'a tree = 
  | Leaf 
  | Node of 'a node

and 'a node = { 
  value: 'a; 
  left:  'a tree; 
  right: 'a tree
}

(* [mem x t] returns [true] if and only if [x] is a value at some
 * node in tree [t].  *)
(* [mem x] funtion a' tree -> bool *)
let rec mem x = function 
    | Leaf -> false
    | Node {value; left; right} -> value=x || mem x left || mem x right ;;

let t =
  Node {
    value = 2; 
    left  = Node {value=1; left=Leaf; right=Leaf};
    right = Node {value=3; left=Leaf; right=Leaf}  
  }



