open Common

let () =
    let file = open_in "./day1/input.txt" in
    let lines = Util.get_lines file in
    let parse_line line =
        Scanf.sscanf line "%d %d" (fun l r -> (l, r)) in
    let pairs = List.map parse_line lines in
    let left = List.map (fun p -> match p with | (l, _) -> l) pairs in
    let right = List.map (fun p -> match p with | (_, r) -> r) pairs in
    let sum = List.fold_left2 (fun init a b -> init + Int.abs (a - b)) 0 (List.sort Int.compare left) (List.sort Int.compare right) in
    print_endline (string_of_int sum)
