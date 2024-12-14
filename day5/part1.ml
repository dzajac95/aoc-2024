open Common

let () =
    let combine_same input = 
        let rec proc input acc =
            match input with
            | [] -> acc
            | (x,v)::xs ->
            if List.mem_assoc x acc then
                let prev = List.assoc x acc in
                proc xs ((x,v::prev) :: (List.remove_assoc x acc))
            else
                proc xs ((x,[v]) :: acc) in
        proc input [] in

    let lines = Util.get_lines "./day5/test.txt" in
    let parse_rules ls =
        let parse line =
            match Str.split (Str.regexp "|") line with
            | [l; r] -> (int_of_string l, int_of_string r)
            | _ -> raise (Invalid_argument ("bad input: " ^ line)) in
        let rec loop ls (acc: (int * int) list) =
            match ls with
            | [] -> raise (Failure "shouldn't happen")
            | l::rest ->
                if l = "" then
                    (acc, rest)
                else
                    loop rest ((parse l)::acc) in
        loop ls [] in
    let raw_rules, rem = parse_rules lines in
    let proc_rule rule = 
            List.map (fun y -> (fun x -> x == y)) rule
            |> List.fold_left (fun acc f -> (fun x -> acc x || f x)) (fun _ -> false) in
    let ruledata = combine_same raw_rules  in
    let rules = List.map (fun (x,rule) -> x,proc_rule rule) ruledata in
    print_endline "Parsed rules:";
    List.iter (fun (x,v) -> print_string ((string_of_int x) ^ ": "); Util.print_list string_of_int v) ruledata;
    let parse_updates ls =
        let parse line =
            List.map int_of_string Str.(split (regexp ",") line) in
        List.map parse ls in
    let updates = parse_updates rem in
    (* List.iter print_list string_of_int updates; *)
    let check_update update = 
        let rec loop rem =
            match rem with
            | [] -> true
            | _::[] -> true
            | x::xs ->
                    if List.mem_assoc x rules then
                        let found = List.map (List.assoc x rules) xs
                                    |> List.fold_left ( || ) false in
                        if found then
                            false
                        else
                            loop xs
                    else loop xs in

        loop (List.rev update) in
    let res = List.filter check_update updates
              |> List.map (fun l -> List.nth l ((List.length l) / 2))
              |> List.fold_left ( + ) 0 in
    print_endline ("Final result: " ^ string_of_int res)
