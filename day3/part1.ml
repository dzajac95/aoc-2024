open Common

let () =
    let file = "./day3/input.txt" in
    let content = Util.read_entire_file file in
    let rg = Str.regexp {|mul([0-9][0-9]?[0-9]?,[0-9][0-9]?[0-9]?)|} in
    let find_all () =
        let rec loop pos acc = 
            try
                let match_idx = Str.search_forward rg content pos in
                let match_str = Str.matched_string content in
                loop (match_idx + (String.length match_str)) (match_str::acc)
            with
            | Not_found -> List.rev acc
            | err -> raise err in
        loop 0 [] in
    let matches = find_all () in
    print_endline content;
    print_endline "Matches:";
    List.iter (fun s -> print_endline s) matches;
    let calculate s =
        let num_str = String.sub s 4 ((String.length s) - 4 - 1) in
        match Str.split (Str.regexp ",") num_str with
        | [l; r] -> (int_of_string l) * (int_of_string r)
        | _ -> raise (Invalid_argument num_str) in
    let total = List.fold_left (fun acc x -> acc + x) 0 (List.map calculate matches) in
    print_endline ("Total sum: " ^ (string_of_int total))
