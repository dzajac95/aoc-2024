let get_lines ic =
    let rec loop acc =
        try
            let line = input_line ic in
            loop (line :: acc)
        with
        | End_of_file -> List.rev acc
        | e -> raise e in
    loop []

let read_entire_file filepath =
    let file = open_in filepath in
    List.fold_left (fun acc s -> acc ^ s) "" (get_lines file)
