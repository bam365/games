open Core.Std

type t = (Card.face * int) list 

let create () = List.map ~f:(fun cf -> cf, 0) Card.all_faces 

let face_count board cf = List.Assoc.find_exn board cf

let add_card board = function
    | None -> board
    | Some card ->
        let cf = Card.face card in
        let count = face_count board cf in
        if count < 4 then List.Assoc.add board cf (count + 1) else board
        
let is_full = List.for_all ~f:(fun (_, count) -> count >= 4) 
