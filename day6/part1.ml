open Common

module IntPairs =
    struct
        type t = int * int
        let compare (x0,y0) (x1,y1) =
            match compare x0 x1 with
            | 0 -> compare y0 y1
            | c -> c
    end

module PairsSet = Set.Make(IntPairs)

type dir = L | R | U | D

let dir_off = function
    | L -> (-1, 0)
    | R -> (1, 0)
    | U -> (0, -1)
    | D -> (0, 1)

let dir_next = function
    | L -> U
    | R -> D
    | U -> R
    | D -> L

let () =
    let grid = Util.get_grid "./day6/input.txt" in
    Util.grid_print grid;
    let (start_x, start_y) = Util.grid_coord grid (String.index grid.data '^') in
    print_endline (Printf.sprintf "Starting pos: %d,%d" start_x start_y);
    let walk g (x,y) =
        let rec loop x y dir visited =
            try
                let (off_x, off_y) = dir_off dir in
                let c = Util.grid_at g (x+off_x) (y+off_y) in
                match c with
                | "#" -> loop x y (dir_next dir) visited
                | _ -> loop (x+off_x) (y+off_y) dir (PairsSet.add (x,y) visited)
            with
            | Invalid_argument _ -> (PairsSet.add (x,y) visited) in
        loop x y U PairsSet.empty in
    let visited_idxs = walk grid (start_x, start_y) in
    Printf.printf "Distinct positions: %d\n" (List.length (PairsSet.to_list visited_idxs));
