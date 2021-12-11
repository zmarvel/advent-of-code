(*
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
        let direction =
            matrix_sub grid (matrix_sub north (matrix_sub south (matrix_sub east west)))
        in
        print_matrix direction;
        Printf.printf "\n";
        let north_gt = matrix_less_than2 grid north in
        let south_gt = matrix_less_than2 grid south in
        let east_gt = matrix_less_than2 grid east in
        let west_gt = matrix_less_than2 grid west in
        let lower_count = 
            matrix_add north_gt (matrix_add south_gt (matrix_add west_gt east_gt))
        in
        print_matrix lower_count;
        Printf.printf "\n";
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
    (* process_file "9.input"; *)
;;
