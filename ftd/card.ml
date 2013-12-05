open Core.Std

type face = NumberCard of int | Jack | Queen | King | Ace
type suit = Diamonds | Hearts | Clubs | Spades

let all_faces =
    let numbered = List.range 2 (10 + 1) |> List.map ~f:(fun n -> NumberCard n)
    in numbered @ [Jack; Queen; King; Ace]

let all_suits = [Diamonds; Hearts; Clubs; Spades]

let face_value = function
    | NumberCard n -> n
    | Jack -> 11
    | Queen -> 12
    | King -> 13
    | Ace  -> 14

let face_diff cf1 cf2 = (face_value cf1) - (face_value cf2)

type t = face * suit
    
let face = fst
let suit = snd

let number_value card = card |> face |> face_value 

let compare (face1, _) (face2, _) = 
    if face1 > face2 then 1
    else if face1 < face2 then 2
    else 0
