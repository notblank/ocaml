module type Map = sig

    (** [('k, 'v) t] is the type of maps that bind keys of type ['k] to values
     * of type ['v]. *)
    type ('k, 'v) t

    (** [insert k v m] is the map [m] with [k] binding to [v]. Previous
     * bindings of [k] are replaced by [v]. *)
    val insert : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t

    (** [find k m] is [Some v] if [k] is bound to [v] in [m] and [None] if [k]
     * has no binding in [m].*)
    val find : 'k -> ('k, 'v) t -> 'v option 

    (** [remove k m] is the map [m] without the binding of [k]. Leaves [m]
     * unchanged when [k] has no bindings in [m]. *)
    val remove : 'k -> ('k, 'v) t -> ('k, 'v) t

    (**  [empty] is the empty map. *)
    val empty : ('k, 'v) t
     
    (** [of_list lst] is a map containing the bindings in the association list.
     * Requires: [lst] does not contain duplicate keys. *)
    val of_list : ('k * 'v) list -> ('k, 'v) t

    (** [bindings m] is an association list containing the same bindings as
     * [m]. The resulting list contains no duplicates.*)
    val bindings : ('k, 'v) t -> ('k * 'v) list

end


(** maps as associations lists *)

module AssocListMap : Map = struct


    (** Abstraction Function: [(k1, v1); ... ; (kn, vn)] is the map 
     * {k1:v1, ... , kn:vn}. If a key is bound to more than one value, the map
     * will contain the left-most binding. For example: [(k, v1); (k, v2)] is
     * the map {k:v1}. The empty map is [].
     * Representation Invariant: none
     * *)
    type ('k, 'v) t = ('k * 'v) list

    let insert _ _ _ =
        failwith "unimplemented"

    let find _ _ =
        failwith "unimplemented"

    let remove _ _ =
        failwith "unimplemented"

    let empty = []

    let of_list lst =
        lst

    (** [keys m] is a list of the keys in [m] without duplicates.
     * Efficiency: O(n log n).*)
    let keys m =
        m |> List.map fst |> List.sort_uniq Stdlib.compare

    (** [binding m k] is [(k, v)] where [v] is the value that [k] binds in [m].
     * Requires: [k] is a key in [m]. *)
    let binding m k =
        (k, List.assoc k m)

    let bindings m =
        List.map (binding m) (keys m) 
end



