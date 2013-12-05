type ftd_state = FirstGuess | SecondGuess | DealerChoice | Finished
type first_guess_result  = GuessTooHigh | GuessTooLow | Correct | NotAllowed
type second_guess_result = Missed of int | Correct | NotAllowed

type t = 
    { players: string list;
      dealers: string list;      
      state: ftd_state;
      turns: int;
      card:  Card.t option;
      deck:  Deck.t;
      board: Board.t;
    }
    

let dealer_yield_count = 3

let rotate = function
    | [] -> []
    | head::tail -> tail @ [head]

let head_or_empty_str = function
    | []      -> ""
    | head::_ -> head


let create ?random_state = function 
    | [] | [_] -> None
    | players -> 
        let (first_card, init_deck) = 
            Deck.create () |> Deck.shuffle ?random_state |> Deck.draw_card
        in
        Some { players = players;
               dealers = List.rev players;
               state = FirstGuess;
               turns = 0;
               card = first_card;
               deck = init_deck;
               board = Board.create ();
             }


let current_dealer game = head_or_empty_str game.dealers 
let current_player game = head_or_empty_str game.players
let current_card game = game.card
let current_state game = game.state
let dealer_turns game = game.turns

let cycle_player game = { game with players = (rotate game.players) }


let cycle_player_if_dealer game =
    if (current_player game) = (current_dealer game) 
        then (cycle_player game)
        else game

let cycle_turn game was_correct = 
    let old_card = game.card in 
    let new_turns = if was_correct then 0 else (game.turns + 1) in
    let new_card, new_deck = Deck.draw_card game.deck in
    let new_state = 
        match new_card with 
        | None -> Finished
        | Some card ->
            if new_turns >= dealer_yield_count 
                then DealerChoice
                else FirstGuess
    in
    let game_new_player = cycle_player game |> cycle_player_if_dealer in
    { game_new_player with state = new_state;
                           turns = new_turns;  
                           card  = new_card;
                           deck  = new_deck;
                           board = Board.add_card game.board old_card;
    }


let cycle_dealer game = 
    { game with dealers = (rotate game.dealers);
                state = FirstGuess;
                turns = 0;
    }
    |> cycle_player_if_dealer

let dealer_yield game =
    match (current_state game) with
    | FirstGuess | SecondGuess | Finished -> false, game
    | DealerChoice -> true, (cycle_dealer game)


let diff_game_card_with_face game card_face =
    match game.card with
    | Some card -> Card.face_diff card_face (Card.face card)
    | None -> 0 (* doesn't really matter, does it? *)


let first_guess game card_face =
    match (current_state game) with
    | DealerChoice | FirstGuess ->
        let diff = diff_game_card_with_face game card_face in
        if diff < 0 then GuessTooLow, game
        else if diff > 0 then GuessTooHigh, game
        else Correct, (cycle_turn game true)
    | SecondGuess | Finished -> (NotAllowed : first_guess_result), game


let second_guess game card_face = 
    match (current_state game) with
    | SecondGuess ->
        let diff = diff_game_card_with_face game card_face in
        if diff = 0 then Correct, (cycle_turn game true)
        else Missed (abs diff), (cycle_turn game false)
    | FirstGuess | DealerChoice | Finished -> (NotAllowed : second_guess_result), game

