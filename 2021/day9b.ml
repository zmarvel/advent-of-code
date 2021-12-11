(*
--- Part Two ---

Next, you need to find the largest basins so you know what areas are most important to avoid.

A basin is all locations that eventually flow downward to a single low point. Therefore, every low point has a basin, although some basins are very small. Locations of height 9 do not count as being in any basin, and all other locations will always be part of exactly one basin.

The size of a basin is the number of locations within the basin, including the low point. The example above has four basins.

The top-left basin, size 3:

2199943210
3987894921
9856789892
8767896789
9899965678

The top-right basin, size 9:

2199943210
3987894921
9856789892
8767896789
9899965678

The middle basin, size 14:

2199943210
3987894921
9856789892
8767896789
9899965678

The bottom-right basin, size 9:

2199943210
3987894921
9856789892
8767896789
9899965678

Find the three largest basins and multiply their sizes together. In the above example, this is 9 * 14 * 9 = 1134.

What do you get if you multiply together the sizes of the three largest basins?
*)


open Utils;;


(* Load the matrix into an array of strings *)
let load_file inc =
    (* Load the rows in reverse, then reverse it at the end *)
    let rec helper rows =
        match input_line_opt inc with
        | Some(line) ->
                let row = Array.map ctoi (array_of_string (String.trim line)) in
                helper (row :: rows)
        | None -> rows
    in
    reverse_array (Array.of_list (helper []))
;;


let add_pair (x1, y1) (x2, y2) = 
    (x1 + x2, y1 + y2)
;;


let valid_position dims point =
    let dim1, dim2 = dims in
    let i, j = point in
    let valid = i >= 0 && j >= 0 && i < dim1 && j < dim2 in
    valid
;;


(** Start at [i, j] and return a list of neighboring (recusively) positions equal to 1.
   *)
let flood_fill (m : int array array) i j =
    let dims = get_matrix_dims m in
    let num_rows, num_cols = dims in
    let flooded = Array.make_matrix num_rows num_cols 0 in
    let directions = [ (-1, 0); (0, 1); (1, 0); (0, -1) ] in
    let rec fill_neighbors i j =
        let valid = valid_position dims (i, j) in
        (* If it's in bounds *)
        if not valid then ()
        (* And not already marked *)
        else if flooded.(i).(j) = 1 then ()
        (* And in the mask *)
        else if m.(i).(j) = 0 then ()
        else
            (* Mark the spot *)
            (flooded.(i).(j) <- 1;
            List.iter (fun dir ->
                let i', j' = add_pair (i, j) dir in
                fill_neighbors i' j') directions;)
    in
    fill_neighbors i j;
    matrix_where (fun x -> x = 1) flooded
;;


let do_game grid =
    let north = matrix_slide_up 1 10 grid in
    let south = matrix_slide_down 1 10 grid in
    let east = matrix_slide_right 1 10 grid in 
    let west = matrix_slide_left 1 10 grid in
    let north_gt = matrix_less_than2 grid north in
    let south_gt = matrix_less_than2 grid south in
    let east_gt = matrix_less_than2 grid east in
    let west_gt = matrix_less_than2 grid west in
    let lower_count = 
        matrix_add north_gt (matrix_add south_gt (matrix_add west_gt east_gt))
    in
    let low_points = matrix_equal lower_count 4 in
    let not_peaks = matrix_equal (matrix_less_than grid 9) 1 in
    let basins =
        let num_rows, num_cols = get_matrix_dims grid in
        let rec loop i j acc =
            if i = num_rows then acc
            else if j = num_cols then loop (succ i) 0 acc
            else
                if low_points.(i).(j) = 1 then
                    let basin = flood_fill not_peaks i j in
                    loop i (succ j) (basin :: acc)
                else
                    loop i (succ j) acc
        in
        loop 0 0 []
    in
    let basin_sizes =
        Array.of_list (List.map List.length basins)
    in
    Array.sort compare basin_sizes;
    let top_basin_sizes = Array.sub basin_sizes ((Array.length basin_sizes) - 3) 3 in
    Array.fold_left ( * ) 1 top_basin_sizes
;;


let process_file filename =
    let inc = open_in filename in
    let grid = load_file inc in
    Printf.printf "input=";
    print_matrix grid;
    Printf.printf"\n";
    let result = do_game grid in
    Printf.printf "filename=%s\n" filename;
    Printf.printf "result=%d\n" result;

    Printf.printf "\n";
;;

let () =
    process_file "9-test.input";
    process_file "9.input";
;;
