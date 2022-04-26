
let counter = ref 0;;

let next () = 
    counter := !counter + 1; (** add 1, can use incr function *)
    !counter (** return new content *);;
        
next ();; (* adds 1 to content *)
