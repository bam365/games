open Core.Std

type face = NumberCard of int | Jack | Queen | King | Ace
type suit = Diamonds | Hearts | Clubs | Spades

val all_faces : face list
val all_suits : suit list
val face_value : face -> int
val face_diff : face -> face -> int

type t = face * suit


val face : t -> face
val suit : t -> suit
val number_value : t -> int
val compare : t -> t -> int

    
    
