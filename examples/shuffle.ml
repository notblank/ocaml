module type SHUFFLE = sig
    type a' t
    val play_card : 'a -> 'a t -> 'a
end

let rec repeat m n l = 
    match n with
    | 0 -> l
    | n -> repeat m (n - 1) (m::l) ;;


(* deal_card [card_num] of deck [d] to [player_num] of [num_players]*)
let deal_card d player_num num_players card_num =
    List.nth d (player_num + (num_players*card_num));;

let rec deal_cards d player_num num_players num_cards hand =
    match num_cards with
    | -1 -> hand 
    | num_cards -> 
            let card = deal_card d player_num num_players num_cards in
            deal_cards d player_num num_players (num_cards - 1) (card::hand);;

(* mem_i returns index of first element [a] in [l] *)
let mem_i a l =
    let n = List.length l in
    let rec mem_i_aux a l n =
        match l with
        | [] -> -1
        | h::t -> if h = a then n else (mem_i_aux a t (n - 1)) in
    let nn = mem_i_aux a l n in
    if nn = -1 then nn else (n - nn);;

let play_card card hand = 
    let n = (mem_i card hand) in
    let nc = (List.length hand) in
    if n = -1 then 
        (List.nth hand (Random.int (nc - 1)))
    else 
        (List.nth hand n);;

play_card "A" ["A"; "C"; "D"];;
play_card "F" ["A"; "C"; "D"];;




