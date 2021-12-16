(*
--- Day 15: Chiton ---
You've almost reached the exit of the cave, but the walls are getting closer together. Your submarine can barely still fit, though; the main problem is that the walls of the cave are covered in chitons, and it would be best not to bump any of them.

The cavern is large, but has a very low ceiling, restricting your motion to two dimensions. The shape of the cavern resembles a square; a quick scan of chiton density produces a map of risk level throughout the cave (your puzzle input). For example:

1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581
You start in the top left position, your destination is the bottom right position, and you cannot move diagonally. The number at each position is its risk level; to determine the total risk of an entire path, add up the risk levels of each position you enter (that is, don't count the risk level of your starting position unless you enter it; leaving it adds no risk to your total).

Your goal is to find a path with the lowest total risk. In this example, a path with the lowest total risk is highlighted here:

1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581
The total risk of this path is 40 (the starting position is never entered, so its risk is not counted).

What is the lowest total risk of any path from the top left to the bottom right?
*)


open Utils;;


exception Invalid_input_file of string


let dijkstra nodes =
    ()
;;


(* Load a list of points followed by a list of folds *)
let load_file inc =
    let rec load_template template =
        match input_line_opt inc with
        | Some(line) -> (
                Printf.printf "line=%s\n" line;
                let line = String.trim line in
                if String.length line > 0 then
                    load_template line
                else template)
        | None -> raise (Invalid_input_file "Expected replacements after template")
    in
    let template = load_template "" in
    (* Just load replacements into a list of 3-tuples *)
    let rec load_replacements replacements =
        match input_line_opt inc with
        | Some (line) -> (
                Printf.printf "line=%s\n" line;
                let replacement = line
                    |> String.trim
                    |> String.split_on_char ' '
                    |> (function
                            | pair :: _arrow :: between :: [] ->
                                    (pair.[0], pair.[1], between.[0])
                            | _ -> raise (Invalid_input_file "Unexpected replacement format"))
                in
                load_replacements (replacement :: replacements))
        | None -> replacements
    in
    let replacements = Array.of_list (load_replacements []) in
    (template, replacements)
;;

let process_file filename =
    let inc = open_in filename in
    Printf.printf "filename=%s\n" filename;

    Printf.printf "\n";
;;


let () =
    process_file "15-test.input";
    (*
    process_file "15.input";
    *)
;;
