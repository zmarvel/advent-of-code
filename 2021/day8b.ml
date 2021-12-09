(*
--- Part Two ---
Through a little deduction, you should now be able to determine the remaining digits. Consider again the first example above:

acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab |
cdfeb fcadb cdfeb cdbaf
After some careful analysis, the mapping between signal wires and segments only make sense in the following configuration:

 dddd
e    a
e    a
 ffff
g    b
g    b
 cccc
So, the unique signal patterns would correspond to the following digits:

acedgfb: 8
cdfbe: 5
gcdfa: 2
fbcad: 3
dab: 7
cefabd: 9
cdfgeb: 6
eafb: 4
cagedb: 0
ab: 1
Then, the four digits of the output value can be decoded:

cdfeb: 5
fcadb: 3
cdfeb: 5
cdbaf: 3
Therefore, the output value for this entry is 5353.

Following this same process for each entry in the second, larger example above, the output value of each entry can be determined:

fdgacbe cefdb cefbgd gcbe: 8394
fcgedb cgb dgebacf gc: 9781
cg cg fdcagb cbg: 1197
efabcd cedba gadfec cb: 9361
gecf egdcabf bgf bfgea: 4873
gebdcfa ecba ca fadegcb: 8418
cefg dcbef fcge gbcadfe: 4548
ed bcgafe cdgba cbgef: 1625
gbdfcae bgc cg cgb: 8717
fgae cfgab fg bagce: 4315
Adding all of the output values in this larger example produces 61229.

For each entry, determine all of the wire/segment connections and decode the four-digit output values. What do you get if you add up all of the output values?
*)


open Utils;;


(** Load an array of fish positions
 *)
let load_file inc =
    let rec helper entries =
        match input_line_opt inc with
        | Some(line) ->
                let input_output =
                    line
                    |> String.split_on_char '|'
                    |> List.map String.trim
                    (* Make a list of lists, where the first list is the inputs and the second list is the outputs *)
                    |> List.map (String.split_on_char ' ')
                    |> List.map Array.of_list
                in
                let input = List.hd input_output in
                let output = List.nth input_output 1 in
                helper ((input, output) :: entries)
        | None -> entries
    in
    reverse_array (Array.of_list (helper []))
;;


(** Assumes s1 and s2 are the same length. Produces a list of pairs [(s1.[i], s2.[i])] *)
let zip_strings s1 s2 =
    if String.length s1 <> String.length s2 then
        Printf.printf "ERROR: Unmatched strings: %s %s\n" s1 s2;
    let rec loop i =
        if i < 0 then
            []
        else
            (s1.[i], s2.[i]) :: (loop (pred i))
    in
    loop (String.length s1 - 1)
;;


let find_unique_segments (displays : string array) =
    Array.fold_left (fun acc -> fun num ->
        let new_pairs =
            match String.length num with
            (* We need the association in reverse: decode instead of encode *)
            | 2 (* 1 *) -> zip_strings num "cf"
            | 4 (* 4 *) -> zip_strings num "bcdf"
            | 3 (* 7 *) -> zip_strings num "acf"
            | 7 (* 8 *) -> zip_strings num "abcdefg"
            | _ -> []
        in
        (* TODO: How does the order affect complexity here? *)
        List.rev_append new_pairs acc
    ) [] displays
;;

let count_unique_segments (displays : string array) =
    Array.fold_left (fun acc -> fun num ->
        let new_pairs =
            match String.length num with
            | 2 | 4 | 3 | 7 (* 1, 4, 7, 8 *) -> 1
            | _ -> 0
        in
        acc + new_pairs
    ) 0 displays
;;


let do_game entries =
    Array.fold_left (fun acc -> fun entry ->
        let _, output = entry in
        let unique_segments = count_unique_segments output in
        (* let unique_segments = find_unique_segments input in
        (* Now, find the outputs where all the letters are associated in unique_segments *)
        let decodable_outputs =
            Array.fold_left (fun acc -> fun out ->
                (* TODO: If this is too slow, consider searching the array for each unique_segment
                 * instead of the other way around *)
                if string_for_all (fun c -> List.mem_assoc c unique_segments) out then
                    out :: acc
                else
                    acc
            ) [] output
        in
        acc + (List.length decodable_outputs) *)
        acc + unique_segments
    ) 0 entries
;;


let process_file filename =
    let inc = open_in filename in
    let entries = load_file inc in
    let num_decodable = do_game entries in
    Printf.printf "result=%d\n" num_decodable;

    Printf.printf "\n";
;;


(* 588 is too low *)


let () =
    process_file "8-test.input";
    process_file "8.input";
;;
