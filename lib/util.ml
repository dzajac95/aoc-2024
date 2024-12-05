let get_lines ic =
    let rec loop acc =
        try
            let line = input_line ic in
            loop (line :: acc)
        with
        | End_of_file -> List.rev acc
        | e -> raise e in
    loop []
