open Core.Std

type t = Card.t list

let create () = List.cartesian_product Card.all_faces Card.all_suits

let shuffle = List.permute

let is_empty = function
    | []   -> true
    | _::_ -> false

let draw_card = function
    | []   -> (None, [])
    | first_card::new_deck -> (Some first_card, new_deck)

let cards_left = List.length
