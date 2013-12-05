open Core.Std

type card_face = Numbered of int | Jack | Queen | King | Ace
type card_suit = Diamonds | Hearts | Clubs | Spades
type card = card_face * card_suit

val all_card_faces : card_face list
val all_card_suits : card_suit list
val card_face : card -> card_face
val card_suit : card -> card_suit
val compare : card -> card -> int


module Deck : sig 
    type t

    val create : () -> t
    val shuffle : t -> t
    val draw_card : t -> card * t
    val cards_left : t -> int
end
    
