open Common

let check_report report =
    let l = List.filteri (fun i _ -> i < (List.length report) - 1) report in
    let r = List.tl report in
    let diffs = List.map2 (fun a b -> b - a) l r in
    (List.for_all (fun x -> x < 0) diffs || List.for_all (fun x -> x > 0) diffs) &&
    List.for_all (fun x -> 1 <= x && x <= 3) (List.map (fun x -> Int.abs x) diffs)

let () =
    let file = open_in "./day2/input.txt" in
    let lines = Util.get_lines file in
    let parse_line line =
        List.map (fun s -> int_of_string s) (String.split_on_char ' ' line) in
    let reports = List.map parse_line lines in
    let print_report report result =
        let rec loop rem =
            match rem with
            | [] -> ""
            | [x] -> string_of_int x
            | x::rest -> (string_of_int x) ^ " " ^ (loop rest) in
        print_string "[";
        print_string (loop report);
        print_string "] -> ";
        if result then
            print_endline "GOOD"
        else
            print_endline "BAD" in
    let report_results = List.map check_report reports in
    List.iter2 print_report reports report_results;
    let total_safe = List.fold_left (fun acc _ -> acc + 1) 0 (List.filter (fun x -> x) report_results) in
    print_endline ("Total safe reports: " ^ (string_of_int total_safe))
