open Core.Std

let print_ln str =
    Out_channel.output_string stdout str;
    Out_channel.output_string stdout "\n";
    Out_channel.flush stdout


let get_players_from_stdin () =
    print_ln "Enter names of players, separated by commas:";
    match In_channel.input_line stdin with
    | Some str -> 
        String.split_on_chars ~on:[','] str
        |> List.map ~f:String.strip
    | None -> []


let face_str card_face =
    let letter = 
        match card_face with
        | Card.NumberCard n -> sprintf "%d" n 
        | Card.Jack -> "J" | Card.Queen -> "Q" | Card.King -> "K" | Card.Ace  -> "A"
    in
    sprintf "%2s" letter


let board_str counts = List.fold_left ~init:"" ~f:(fun acc (card_face, count) ->
    let count_ch = if count < 4 then sprintf "%d" count else "*" in
    let count_str =
        match card_face with 
            | Card.NumberCard 8 | Card.Ace -> sprintf "%s\n" count_ch
            | _ -> sprintf "%s  " count_ch
        in
        sprintf "%s%s: %s" acc (face_str card_face) count_str
    ) counts



let print_game_summary game =
    let dealer_turns = Game.dealer_turns game in
    [""; 
     sprintf "Dealer: %s" (Game.current_dealer game);
     sprintf "  (has survived %d %s)" 
             dealer_turns 
             (if dealer_turns = 1 then "turn" else "turns");
     (Game.face_counts game) |> board_str;
     sprintf "Current turn: %s" (Game.current_player game);
    ]
    |> List.iter ~f:print_ln


let face_from_str str =
    let letter = String.slice str 0 1 |> String.uppercase in
    if (List.for_all ~f:Char.is_digit (String.to_list str)) then
        let num = int_of_string str in
        if num >= 2 && num <= 10 then Some (Card.NumberCard num) else None
    else match letter with
        | "J" -> Some Card.Jack
        | "Q" -> Some Card.Queen
        | "K" -> Some Card.King
        | "A" -> Some Card.Ace
        | _   -> None


let rec get_input_line prompt =
    Out_channel.output_string stdout prompt;
    Out_channel.flush stdout;
    match In_channel.input_line stdin with
    | None -> get_input_line prompt
    | Some str -> str


let rec get_face_guess prompt =
    match (get_input_line prompt |> face_from_str) with 
    | None ->
            print_ln "I don't know what card you mean, try again...";
            get_face_guess prompt
    | Some card_face -> card_face


let rec get_yes_or_no prompt =
    match (String.slice (get_input_line prompt) 0 1 |> String.uppercase) with
    | "Y" -> true
    | "N" -> false
    | _ -> 
        print_ln "I need a yes or no answer...";
        get_yes_or_no prompt


let do_guess_turn ~game ~prompt_phrase ~guess_func ~res_func =
    let prompt = sprintf "%s, %s: " (Game.current_player game) prompt_phrase in
    let guess = get_face_guess prompt in
    let result, new_game = guess_func game guess in
    res_func result |> print_ln;
    new_game


let do_first_guess game =
    do_guess_turn ~game ~prompt_phrase:"guess a card" ~guess_func:Game.first_guess
        ~res_func:(function
            | Game.GuessTooHigh -> "That's too high"
            | Game.GuessTooLow  -> "That's too low"
            | (Game.Correct: Game.first_guess_result) -> 
                    sprintf "Correct! %s, take 10!" (Game.current_dealer game)
            | (Game.NotAllowed: Game.first_guess_result) -> 
                    "You weren't allowed to guess just then..."
        )

        
let do_second_guess game =
    do_guess_turn ~game ~prompt_phrase:"guess another card" ~guess_func:Game.second_guess
        ~res_func:(function 
            | Game.Missed  (card_face, num_drinks) -> 
                sprintf "\nNope. It was a %s, take %d" (face_str card_face) num_drinks 
            | Game.Correct -> sprintf "\nCorrect! %s, take 5!" (Game.current_dealer game)
            | Game.NotAllowed -> "You weren't allowed to guess just then..."
        )


let do_dealer_pass game = 
    let dealer_name = Game.current_dealer game in
    sprintf "%s, would you like to pass to the next dealer (y or n)? " dealer_name
    |> get_yes_or_no
    |> Game.dealer_yield game
    |> snd

   

let do_turn game =
    match (Game.current_state game) with
    | Game.FirstGuess ->
            print_game_summary game;
            do_first_guess game
    | Game.SecondGuess -> do_second_guess game
    | Game.DealerChoice -> do_dealer_pass game
    | Game.Finished -> game
            

let rec play_game game =
    match (Game.current_state game) with
    | Game.Finished -> print_ln "Game over!"
    | _ -> do_turn game |> play_game


let rec start_game () =
    let random_state = Random.State.make_self_init () in
    match get_players_from_stdin () |> Game.create ~random_state with
    | None -> 
            print_ln "Could not create game, try again";
            start_game ()
    | Some game -> 
            print_ln "Starting game...";
            play_game game


let () = start_game ()
