
let n_players = 3;;
let n_cards = 1*n_players;;

let d0 = (repeat "A" n_cards []) @ 
            (repeat "B" n_cards []) @
            (repeat "C" n_cards []) @
            (repeat "D" n_cards []);;

mem_i "A" ["B"; "C"; "C"];;
mem_i "A" ["C"; "A"; "A"];;

