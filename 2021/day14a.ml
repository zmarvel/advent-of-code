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
                        Printf.printf "%s\n" (format_replacement replacement);
                        loop (snd :: rst) (fst :: c :: snd :: chars)
                | None ->
                        loop (snd :: rst) (fst :: snd :: chars'))
        | hd :: rst -> loop rst (hd :: chars')
        | _ -> join_chars chars'
    in
    loop (explode_chars template) []
;;




let process_file filename =
    let inc = open_in filename in
    Printf.printf "filename=%s\n" filename;
    let template, replacements = load_file inc in

    Printf.printf "template=%s\n" template;
    Printf.printf "replacements=%s\n" (format_array replacements format_replacement);

    let first_replaced = do_step template replacements in
    Printf.printf "first_replaced=%s\n" first_replaced;

    Printf.printf "\n";
;;


let () =
    process_file "14-test.input";
    (*
    process_file "14.input";
    *)
;;
