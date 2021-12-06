(*
--- Part Two ---

Suppose the lanternfish live forever and have unlimited food and space. Would they take over the entire ocean?

After 256 days in the example above, there would be a total of 26984457539 lanternfish!

How many lanternfish would there be after 256 days?
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
    | None -> [| |]
;;


(** Slide all elements in a to the left one slot (creates a new array).
    *)
let array_slide_left a =
    let n = Array.length a in
    Array.mapi (fun i -> fun x ->
        if i < n - 1 then
            a.(i + 1)
        else
            x
    ) a
;;



(** Run the simulated game, looping through all the fish each iteration until the desired day is
    reached. [ages] is an array of fish ages, and [days] is how many days should be simulated.
 *)
let do_game ages days =
    let rec day_loop i ages =
        if i = days then
            ages
        else
            let num_new = ages.(0) in
            let ages' = array_slide_left ages in
            ages'.(6) <- ages'.(6) + num_new;
            ages'.(8) <- num_new;
            (* Printf.printf "ages=%s ages'=%s num_new=%d\n" (format_array_of_int ages) (format_array_of_int ages') num_new; *)
            day_loop (succ i) ages'
    in
    let final_ages = day_loop 0 ages in
    array_sum final_ages
;;


let process_file filename =
    Printf.printf "filename=%s\n" filename;
    let inc = open_in filename in
    let initial_ages = load_file inc in
    let ages = Array.fold_left (fun acc -> fun age ->
            acc.(age) <- (acc.(age) + 1);
            acc
        ) (Array.make 9 0) initial_ages
    in
    let answer = do_game ages 256 in

    Printf.printf "answer=%d\n" answer;
    Printf.printf "\n";
;;


let () =
    process_file "6-test.input";
    process_file "6.input";
;;
