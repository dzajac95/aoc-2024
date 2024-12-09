open Common

let () =
    let grid = Util.get_grid "./day4/input.txt" in
    Util.grid_print grid;
    let check_dir g x y off dir =
        let x_off = fst off in
        let y_off = snd off in
        let rec get_s i acc =
            if i < 3 then
                get_s (i+1) (acc ^ Util.grid_at g (x+x_off+i*(fst dir)) (y+y_off+i*(snd dir)))
            else
                acc in
        try
            (get_s 0 "")
        with
        | Invalid_argument _ -> ""
        | _ -> raise (Failure "unexpected exception") in

    let is_xmas g x y =
        if Util.grid_at g x y <> "A" then
            false
        else
        let se = (check_dir g x y (-1, -1) (1, 1)) in
        let ne = (check_dir g x y (-1, 1) (1, -1)) in
        let sw = (check_dir g x y (1, -1) (-1, 1)) in
        let nw = (check_dir g x y (1,  1) (-1, -1)) in
        (se = "MAS" && ne = "MAS") ||
        (se = "MAS" && sw = "MAS") ||
        (sw = "MAS" && nw = "MAS") ||
        (nw = "MAS" && ne = "MAS") in
                     

    let total_xmas = List.fold_left (fun acc x -> acc + x) 0 (Util.grid_map (fun x y _ -> Bool.to_int (is_xmas grid x y)) grid) in
    print_endline ("Total xmas: " ^ string_of_int total_xmas)

