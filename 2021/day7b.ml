(*
--- Part Two ---
The crabs don't seem interested in your proposed solution. Perhaps you misunderstand crab engineering?

As it turns out, crab submarine engines don't burn fuel at a constant rate. Instead, each change of 1 step in horizontal position costs 1 more unit of fuel than the last: the first step costs 1, the second step costs 2, the third step costs 3, and so on.

As each crab moves, moving further becomes more expensive. This changes the best horizontal position to align them all on; in the example above, this becomes 5:

Move from 16 to 5: 66 fuel
Move from 1 to 5: 10 fuel
Move from 2 to 5: 6 fuel
Move from 0 to 5: 15 fuel
Move from 4 to 5: 1 fuel
Move from 2 to 5: 6 fuel
Move from 7 to 5: 3 fuel
Move from 1 to 5: 10 fuel
Move from 2 to 5: 6 fuel
Move from 14 to 5: 45 fuel
This costs a total of 168 fuel. This is the new cheapest possible outcome; the old alignment position (2) now costs 206 fuel instead.

Determine the horizontal position that the crabs can align to using the least fuel possible so they can make you an escape route! How much fuel must they spend to align to that position?
*)


open Utils;;


(** Load an array of fish positions
 *)
let load_file inc =
    match input_line_opt inc with
    | Some(line) ->
            line
            |> String.trim
            |> String.split_on_char ','
            |> Array.of_list
            |> Array.map int_of_string
    | None -> Array.make 0 0
;;


(* Does it make sense to start near the median? (If the answer is in the dataset, then the median
 *is* the answer: see geometric median.) See also Weiszfeld's algorithm. The most basic solution is
 to "try them all" (brute-force).

 An observation wrt the median idea is that there are two possibilities: the minimal-distance point
 is already in the list, or it isn't. But it must be between the smallest and largest in the list,
 so we can use those values as loop boundaries.
 *)


let fuel_cost distance =
    distance * (distance + 1) / 2
;;


let do_game crabs =
    Array.sort compare crabs;
    let n = Array.length crabs in
    let start = crabs.(0) in
    let stop = crabs.(n - 1) in
    Printf.printf "start=%d stop=%d crabs=%s\n" start stop (format_array_of_int crabs);
    let rec loop i total_distance =
        if i > stop then
            total_distance
        else
            let distance_to = Array.map fuel_cost (array_abs (array_sub_scalar crabs i)) in
            let total_distance' = array_sum distance_to in
            (* Printf.printf "i=%d distance_to=%s total_distance=%d total_distance'=%d\n" i
             (format_array_of_int distance_to) total_distance total_distance'; *)
            loop (succ i) (if total_distance' < total_distance then total_distance' else total_distance)
    in
    loop start 999999999
;;


let process_file filename =
    let inc = open_in filename in
    let crabs = load_file inc in
    Printf.printf "filename=%s\n" filename;
    let min_distance = do_game crabs in
    Printf.printf "min_distance=%d\n" min_distance;

    Printf.printf "\n";
;;


(* 588 is too low *)


let () =
    process_file "7-test.input";
    process_file "7.input";
;;
