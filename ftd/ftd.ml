open Core.Std

let print_ln str =
    Out_channel.output_string stdout str;
    Out_channel.flush stdout

let get_players_from_stdin () =
    print_ln "Enter names of players, separated by commas:\n";
    match In_channel.input_line stdin with
    | Some str -> String.split_on_chars ~on:[','] str
    | None -> []




let () =
    let msg = 
        match get_players_from_stdin () |> Game.create with
        | None -> "Could not create game\n"
        | Some _ -> "Game created\n"
    in
    print_ln msg

