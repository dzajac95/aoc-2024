let get_lines filepath =
    let ic = open_in filepath in
    let rec loop acc =
        try
            let line = input_line ic in
            loop (line :: acc)
        with
        | End_of_file -> List.rev acc
        | e -> raise e in
    loop []

let read_entire_file filepath =
    List.fold_left (fun acc s -> acc ^ s) "" (get_lines filepath)

type grid = {
    data : string;
    width : int;
    height : int;
}

let grid_idx g x y =
    if x < 0 || x >= g.width || y < 0 || y >= g.height then
        raise (Invalid_argument ("coords (" ^ string_of_int x ^ "," ^ string_of_int y ^ ") out of bounds"))
    else
        x + y*g.width

let grid_at g x y =
    String.sub g.data (grid_idx g x y) 1

let get_grid filepath =
    let lines = get_lines filepath in
    let width = String.length (List.hd lines) in
    let height = List.length lines in
    if not (List.for_all (fun s -> (String.length s) == width) lines) then
        raise (Failure "line widths do not all match")
    else
        {
            data = List.fold_left (fun acc s -> acc ^ s) "" lines;
            width = width;
            height = height;
        }

let grid_print g =
    let rec print_rows r acc =
        if r < g.height then
            print_rows (r+1) (acc ^ (String.sub g.data (grid_idx g r 0) g.width) ^ "\n")
        else
            acc in
    print_endline (print_rows 0 "")

let grid_iter f g =
    for y = 0 to g.height-1 do
        for x = 0 to g.width-1 do
            f x y (grid_at g x y)
        done
    done

let grid_map f g =
    let rec loop y acc =
        let rec loop_row x acc =
            if x < g.width then
                loop_row (x+1) ((f x y (grid_at g x y)) :: acc)
            else
                List.rev acc in
        if y < g.height then
            loop (y+1) (acc @ (loop_row 0 []))
        else
            acc in
    loop 0 []
