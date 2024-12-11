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
    let rules = combine_same raw_rules in
    List.iter (fun (x, l) -> print_string (string_of_int x ^ ": "); List.iter (fun x -> print_string (string_of_int x ^ " ")) l; print_newline ()) rules;
    let parse_updates ls =
        let parse line =
            List.map int_of_string (Str.split (Str.regexp ",") line) in
        List.map parse ls in
    let updates = parse_updates rem in
    let print_intlist l =
        let rec loop xs =
            match xs with
            | [] -> ()
            | [x] -> print_string (string_of_int x)
            | x::rest -> print_string (string_of_int x ^ ", "); loop rest in
        print_string "[";
        loop l;
        print_endline "]" in
    let check_update rules update =
        let rev_update = List.rev update in
        let rec loop rules rev =
            match rev with
            | [] -> ()
            | x::xs -> 
    List.iter print_intlist updates
        
