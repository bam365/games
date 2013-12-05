open Core.Std

type ftd_state = FirstGuess | SecondGuess | DealerChoice | Finished
type first_guess_result  = GuessTooHigh | GuessTooLow | Correct | NotAllowed
type second_guess_result = Missed of Card.face * int | Correct | NotAllowed

type t

val create : ?random_state:Random.State.t -> string list -> t option
val current_dealer : t -> string
val current_player : t -> string
val current_card : t -> Card.t option
val current_state : t -> ftd_state
val dealer_turns : t -> int
val face_counts : t -> (Card.face * int) list
val dealer_yield : t -> bool -> bool*t
val first_guess : t -> Card.face -> first_guess_result * t
val second_guess : t -> Card.face -> second_guess_result * t

