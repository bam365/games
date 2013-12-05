open Core.Std

type t

val create : unit -> t
val shuffle : ?random_state:Random.State.t -> t -> t
val is_empty : t -> bool
val draw_card : t -> Card.t option * t
val cards_left : t -> int
