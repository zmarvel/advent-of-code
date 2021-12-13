(*
--- Part Two ---

It seems like the individual flashes aren't bright enough to navigate. However, you might have a better option: the flashes seem to be synchronizing!

In the example above, the first time all octopuses flash simultaneously is step 195:

After step 193:
5877777777
8877777777
7777777777
7777777777
7777777777
7777777777
7777777777
7777777777
7777777777
7777777777

After step 194:
6988888888
9988888888
8888888888
8888888888
8888888888
8888888888
8888888888
8888888888
8888888888
8888888888

After step 195:
0000000000
0000000000
0000000000
0000000000
0000000000
0000000000
0000000000
0000000000
0000000000
0000000000

If you can calculate the exact moments when the octopuses will all flash simultaneously, you should be able to navigate through the cavern. What is the first step during which all octopuses flash?
*)


open Utils;;


(* Load the matrix into a list of strings *)
let load_file inc =
    let rec helper lines =
        match input_line_opt inc with
        | Some(line) ->
                let line = line
                    |> String.trim
                    |> array_of_string
                    |> Array.map ctoi
                in
                helper (line :: lines)
        | None -> lines
    in
    helper []
        |> Array.of_list
        |> array_reverse
;;



let do_step counts flashed =
    (* Printf.printf "counts=%s\n" (format_matrix_of_int counts); *)
    let counts = matrix_add_scalar counts 1 in
    let rec do_flashes counts flashed =
        (* Only let an octopus flash if it hasn't flashed already (xor) *)
        let not_flashed = matrix_not flashed in
        let greater_mask = matrix_greater_than counts 9 in
        let new_flashes = matrix_mask greater_mask not_flashed in
        let num_flashing = matrix_sum new_flashes in
        (* Printf.printf "num_flashing=%d new_flashes=%s\n" num_flashing (format_matrix_of_int new_flashes); *)
        let flashed' = matrix_lor2 new_flashes flashed in
        if num_flashing = 0 then
            (* Replace the count for every octopus that flashed with 0 *)
            let final_counts = matrix_mask counts (matrix_not flashed') in
            let num_flashes = matrix_sum (flashed) in
            (num_flashes, final_counts)
        else
            let counts' =
                (* Increment all cells adjacent to a cell that flashed *)
                let new_north = matrix_slide_up 1 0 new_flashes in
                let new_east = matrix_slide_right 1 0 new_flashes in
                let new_south = matrix_slide_down 1 0 new_flashes in
                let new_west = matrix_slide_left 1 0 new_flashes in
                let new_northeast = matrix_slide_right 1 0 new_north in
                let new_southeast = matrix_slide_right 1 0 new_south in
                let new_northwest = matrix_slide_left 1 0 new_north in
                let new_southwest = matrix_slide_left 1 0 new_south in
                List.fold_left matrix_add counts [
                    new_north; new_east; new_south; new_west;
                    new_northeast; new_southeast; new_southwest; new_northwest
                ]
            in
            do_flashes counts' flashed'
    in
    do_flashes counts flashed
;;


let do_game counts =
    let rows, cols = get_matrix_dims counts in
    let total_cells = rows * cols in
    let rec loop i counts =
        let total_flashes, counts' = do_step counts (Array.make_matrix rows cols 0) in
        (* Printf.printf "total_flashes=%d counts'=%s\n" total_flashes (format_matrix_of_int
            counts'); *)
        Printf.printf "i=%d total_flashes=%d\n" i total_flashes;
        (* If all the cells flashed, we have a final answer. Add 1 because it's 1-based. *)
        if total_flashes = total_cells then (succ i)
        else loop (succ i) counts'
    in
    loop 0 counts
;;


let process_file filename =
    let inc = open_in filename in
    let chunks = load_file inc in
    Printf.printf "filename=%s\n" filename;
    let result = do_game chunks in
    Printf.printf "result=%d\n" result;

    Printf.printf "\n";
;;

let () =
    (* process_file "11-test-small.input"; *)
    (* process_file "11-test.input"; *)
    process_file "11.input";
;;
