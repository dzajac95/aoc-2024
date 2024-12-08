open Common

let () =
    let grid = Util.get_grid "./day4/input.txt" in
    Util.grid_print grid;
    let check_dir g x y dir =
        let rec get_s i acc =
            if i < 4 then
                get_s (i+1) (acc ^ Util.grid_at g (x+i*(fst dir)) (y+i*(snd dir)))
            else
                acc in

        try
            (get_s 0 "") = "XMAS"
        with
        | Invalid_argument _ -> false
        | _ -> raise (Failure "unexpected exception") in
    let count_cardinal g x y =
        (Bool.to_int (check_dir g x y (0, -1))) + (*N*)
        (Bool.to_int (check_dir g x y (1, 0)))  + (*E*)
        (Bool.to_int (check_dir g x y (0, 1)))  + (*S*)
        (Bool.to_int (check_dir g x y (-1, 0)))   (*W*) in

    let count_ordinal g x y =
        (Bool.to_int (check_dir g x y (1, -1))) + (*NE*)
        (Bool.to_int (check_dir g x y (1, 1)))  + (*SE*)
        (Bool.to_int (check_dir g x y (-1, 1))) + (*SW*)
        (Bool.to_int (check_dir g x y (-1, -1)))  (*NW*) in

    let cnt = count_cardinal grid 6 4 in
    print_endline ("Cardinal count @ 6,4: " ^ string_of_int cnt);
    let cardinal_counts = List.fold_left (fun acc x -> acc + x) 0 (Util.grid_map (fun x y _ -> count_cardinal grid x y) grid) in
    print_endline ("Total cardinal counts: " ^ string_of_int cardinal_counts);
    let ordinal_counts = List.fold_left (fun acc x -> acc + x) 0 (Util.grid_map (fun x y _ -> count_ordinal grid x y) grid) in
    print_endline ("Total ordinal counts: " ^ string_of_int ordinal_counts);
    let total_xmas = cardinal_counts + ordinal_counts in
    print_endline ("Total xmas: " ^ string_of_int total_xmas)

