
type instruction =
    | Do
    | Dont
    | Mul of int 

let () =
    let file = "./day3/input.txt" in
    let content = Common.Util.read_entire_file file in
    (* let content = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))" in *)
    let rg = Str.regexp {|do()\|don't()\|mul([0-9][0-9]?[0-9]?,[0-9][0-9]?[0-9]?)|} in
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
    let instruction_of_string s =
        match s with
        | "do()" -> Do
        | "don't()" -> Dont
        | _ -> 
            let num_str = String.sub s 4 ((String.length s) - 4 - 1) in
            match Str.split (Str.regexp ",") num_str with
            | [l; r] -> Mul ((int_of_string l) * (int_of_string r))
            | _ -> raise (Invalid_argument num_str) in
    let instrs = List.map instruction_of_string matches in
    let print_instr instr =
        match instr with
        | Do -> print_endline "Do"
        | Dont -> print_endline "Don't"
        | Mul x -> print_endline ("Mul: " ^ string_of_int x) in
    List.iter print_instr instrs;
    let process (instrs: instruction list) =
        let rec loop acc en (instrs: instruction list) =
            match instrs with
            | [] -> acc
            | [instr] -> 
                    (match instr with
                    | Mul x -> acc + x
                    | _ -> acc)
            | i::rest ->
                    match i with
                    | Do -> loop acc true rest
                    | Dont -> loop acc false rest
                    | Mul x -> if en then loop (acc+x) en rest else loop acc en rest in
        loop 0 true instrs in
    let result = process instrs in
    print_endline ("Total: " ^ string_of_int result)

