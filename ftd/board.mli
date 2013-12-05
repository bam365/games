type t

val create : unit -> t
val face_count : t -> Card.face -> int
val add_card : t -> Card.t option -> t
val is_full : t -> bool
