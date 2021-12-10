(*
--- Day 9: Smoke Basin ---
These caves seem to be lava tubes. Parts are even still volcanically active; small hydrothermal vents release smoke into the caves that slowly settles like rain.

If you can model how the smoke flows through the caves, you might be able to avoid it and be that much safer. The submarine generates a heightmap of the floor of the nearby caves for you (your puzzle input).

Smoke flows to the lowest point of the area it's in. For example, consider the following heightmap:

2199943210
3987894921
9856789892
8767896789
9899965678
Each number corresponds to the height of a particular location, where 9 is the highest and 0 is the lowest a location can be.

Your first goal is to find the low points - the locations that are lower than any of its adjacent locations. Most locations have four adjacent locations (up, down, left, and right); locations on the edge or corner of the map have three or two adjacent locations, respectively. (Diagonal locations do not count as adjacent.)

In the above example, there are four low points, all highlighted: two are in the first row (a 1 and a 0), one is in the third row (a 5), and one is in the bottom row (also a 5). All other locations on the heightmap have some lower adjacent location, and so are not low points.

The risk level of a low point is 1 plus its height. In the above example, the risk levels of the low points are 2, 1, 6, and 6. The sum of the risk levels of all low points in the heightmap is therefore 15.

Find all of the low points on your heightmap. What is the sum of the risk levels of all low points on your heightmap?
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


let do_game grid =
    (* Find the places where the cell is less than all its neighbors *)
    let low_points =
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
        matrix_equal lower_count 4
    in
    let risk_levels = matrix_mask (matrix_add_scalar grid 1) low_points in
    matrix_sum risk_levels
;;


let process_file filename =
    let inc = open_in filename in
    let grid = load_file inc in
    let result = do_game grid in
    Printf.printf "filename=%s\n" filename;
    print_matrix grid;
    Printf.printf"\n";
    Printf.printf "result=%d\n" result;

    Printf.printf "\n";
;;

let () =
    process_file "9-test.input";
    process_file "9.input";
;;
