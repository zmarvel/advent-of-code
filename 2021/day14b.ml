(*
--- Part Two ---
Finish folding the transparent paper according to the instructions. The manual says the code is always eight capital letters.

What code do you use to activate the infrared thermal imaging camera system?
*)


open Utils;;


exception Invalid_input_file of string


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


let format_replacement = function
    | (a, b, c) -> Printf.sprintf "%c%c -> %c" a b c
;;


let option_get_or opt default =
    match opt with
    | Some(x) -> x
    | None -> default
;;


let count_chars s =
    let counts = Hashtbl.create 32 in
    String.iter (fun c ->
        match Hashtbl.find_opt counts c with
        | Some(count) -> Hashtbl.replace counts c (succ count)
        | None -> Hashtbl.add counts c 1
    ) s;
    List.of_seq (Hashtbl.to_seq counts)
;;


(** Return a hash table of counts of pairs of characters (polymers) in s. For example, "abba" would
 * result in a mapping of (a, b) to 1, (b, b) to 1, and (b, a) to 1.
 *)
let count_polymers s =
    let chars = explode_chars s in
    let counts = Hashtbl.create 32 in
    let rec loop chars =
        match chars with
        | [] -> counts
        | _ :: [] -> counts
        | fst :: snd :: rst ->
                let curr_count = option_get_or (Hashtbl.find_opt counts (fst, snd)) 0 in
                Hashtbl.replace counts (fst, snd) (succ curr_count);
                loop (snd :: rst)
    in
    loop chars
;;


let format_counts counts =
    let counts_list =
        counts
            |> Hashtbl.to_seq
            |> List.of_seq
    in
    format_list counts_list (fun m ->
        let (a, b), count = m in
        Printf.sprintf "\n%c%c=%d" a b count)
;;

let format_element_counts counts =
    let counts_list =
        counts
            |> Hashtbl.to_seq
            |> List.of_seq
    in
    format_list counts_list (fun m ->
        let a, count = m in
        Printf.sprintf "\n%c=%d" a count)
;;


(** Returns (number of replaced polymers, updated polymer counts).
 *)
let replace_polymer counts counts' replacement =
    let a, b, c = replacement in
    let ab_count = match Hashtbl.find_opt counts (a, b) with
        | Some(count) ->
                let count1 = option_get_or (Hashtbl.find_opt counts' (a, c)) 0 in
                let count2 = option_get_or (Hashtbl.find_opt counts' (c, b)) 0 in
                Hashtbl.replace counts' (a, c) (count + count1);
                Hashtbl.replace counts' (c, b) (count + count2);
                count
        | None -> 0
    in
    ab_count, counts'
;;


let count_elements template =
    let element_counts = Hashtbl.create 32 in
    String.iter (fun c ->
        let ccount = option_get_or (Hashtbl.find_opt element_counts c) 0 in
        Hashtbl.replace element_counts c (succ ccount)
    ) template;
    element_counts
;;


(** Returns (replaced_polymer_counts, element_counts *)
let do_step counts element_counts replacements =
    (* loop through counts and find out if each replacement can be applied *)
    let counts' = Hashtbl.create (Hashtbl.length counts) in
    let n_replacements = Array.length replacements in
    let rec loop i replaced_counts element_counts =
        if i = n_replacements then replaced_counts, element_counts
        else
            let replacement = replacements.(i) in
            let replacement_count, replaced_counts' =
                replace_polymer counts replaced_counts replacement
            in
            let _, _, c = replacement in
            let c_count = option_get_or (Hashtbl.find_opt element_counts c) 0 in
            Hashtbl.replace element_counts c (replacement_count + c_count);
            loop (succ i) replaced_counts' element_counts
    in
    loop 0 counts' element_counts
;;


let do_game template replacements =
    let num_steps = 40 in
    let counts = count_polymers template in
    let rec loop i polymer_counts element_counts =
        if i = num_steps then polymer_counts, element_counts
        else
            let counts', element_counts = do_step polymer_counts element_counts replacements in
            Printf.printf "i=%d counts'=%s\n" i (format_counts counts');
            Printf.printf "element_counts=%s\n" (format_element_counts element_counts);
            loop (succ i) counts' element_counts
    in
    Printf.printf "counts=%s\n" (format_counts counts);
    let _replaced_counts, element_counts = loop 0 (Hashtbl.copy counts) (count_elements template) in
    (* Printf.printf "replaced_counts=%s\n" (format_counts replaced_counts); *)
    let counts = Array.of_seq (Hashtbl.to_seq element_counts) in
    Array.sort (fun x -> fun y ->
        let _, countx = x in
        let _, county = y in
        compare countx county
    ) counts;
    let num_counts = Array.length counts in
    counts.(0), counts.(num_counts - 1)
;;



let process_file filename =
    let inc = open_in filename in
    Printf.printf "filename=%s\n" filename;
    let template, replacements = load_file inc in

    Printf.printf "template=%s\n" template;
    Printf.printf "replacements=%s\n" (format_array replacements format_replacement);

    let min_count, max_count = do_game template replacements in
    let minc, mincount = min_count in
    let maxc, maxcount = max_count in
    Printf.printf "mincount=%d (%c) maxcount=%d (%c)\n" mincount minc maxcount maxc;
    Printf.printf "result=%d\n" (maxcount - mincount);
    (*
    *)

    Printf.printf "\n";
;;


let () =
    process_file "14-test.input";

    (*
    let count_string s =
        Printf.printf "s=%s\n" s;
        Printf.printf "counts=%s %d\n" (format_counts (count_polymers s)) (String.length s);
    in
    count_string "NCNBCHB";
    count_string "NBCCNBBBCBHCB";
    count_string "NBBBCNCCNBBNBNBBCHBHHBCHB";
    *)

    process_file "14.input";
    (*
    *)
;;
