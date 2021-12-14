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
        | Some(line) ->
                let line = String.trim line in
                if String.length line > 0 then
                    load_template line
                else template
        | None -> raise (Invalid_input_file "Expected replacements after template")
    in
    let template = load_template "" in
    (* Just load replacements into a list of 3-tuples *)
    let rec load_replacements replacements =
        match input_line_opt inc with
        | Some (line) ->
                let replacement = line
                    |> String.trim
                    |> String.split_on_char ' '
                    |> (function
                            | pair :: _arrow :: between :: [] ->
                                    (pair.[0], pair.[1], between.[0])
                            | _ -> raise (Invalid_input_file "Unexpected replacement format"))
                in
                load_replacements (replacement :: replacements)
        | None -> replacements
    in
    let replacements = Array.of_list (load_replacements []) in
    (template, replacements)
;;


let format_replacement = function
    | (a, b, c) -> Printf.sprintf "%c%c -> %c" a b c
;;


let do_step template replacements =
    let rec loop (chars : char list) chars' =
        match chars with
        | fst :: snd :: rst ->
                (match array_find_opt (function
                    | (a, b, _) -> a = fst && b = snd) replacements with
                | Some(replacement) ->
                        let _, _, c = replacement in
                        (* Printf.printf "%s\n" (format_replacement replacement); *)
                        loop (snd :: rst) (c :: fst :: chars')
                | None ->
                        loop (snd :: rst) (snd :: fst :: chars'))
        | hd :: rst -> loop rst (hd :: chars')
        | _ -> join_chars chars'
    in
    string_reverse (loop (explode_chars template) [])
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


let do_game template replacements =
    let num_steps = 40 in
    let rec loop i template =
        if i = num_steps then template
        else loop (succ i) (do_step template replacements)
    in
    let replaced = loop 0 template in
    Printf.printf "replaced=%s\n" replaced;
    let counts = Array.of_list (count_chars replaced) in
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

    Printf.printf "\n";
;;


let () =
    process_file "14-test.input";
    process_file "14.input";
    (*
    *)
;;
