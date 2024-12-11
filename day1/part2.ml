open Common

let () =
    let lines = Util.get_lines "./day1/input.txt" in
    let parse_line line =
        Scanf.sscanf line "%d %d" (fun l r -> (l, r)) in
    let pairs = List.map parse_line lines in
    let left = List.map (fun p -> match p with | (l, _) -> l) pairs in
    let right = List.map (fun p -> match p with | (_, r) -> r) pairs in
    let cnt target xs =
        let filt = List.filter (fun x -> x = target) xs in
        List.fold_left (fun acc _ -> acc + 1) 0 filt in
    let scores = List.map (fun x -> x * (cnt x right)) left in
    let total_score = List.fold_left (fun acc x -> acc + x) 0 scores in
    print_endline (string_of_int total_score)
