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
    val bindings : ('k, 'v) t -> ('k, 'v) list

end



