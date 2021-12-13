(*
--- Part Two ---
Finish folding the transparent paper according to the instructions. The manual says the code is always eight capital letters.

What code do you use to activate the infrared thermal imaging camera system?
*)


open Utils;;
open Point;;


exception Invalid_list_length of int


let point_of_list2 ls =
    match ls with
    | fst :: snd :: [] -> Point.make fst snd
    | _ -> raise (Invalid_list_length (List.length ls))
;;


exception Invalid_input_file of string


(* Load a list of points followed by a list of folds *)
let load_file inc =
    let rec load_points lines =
        match input_line_opt inc with
        | Some(line) ->
                let line = String.trim line in
                if String.length line > 0 then
                    let line = line
                        |> String.split_on_char ','
                        |> List.map int_of_string
                        |> point_of_list2
                    in
                    load_points (line :: lines)
                else lines
        | None -> raise (Invalid_input_file "Expected folds after point list")
    in
    let points = load_points [] in
    let rec load_folds folds =
        match input_line_opt inc with
        | Some (line) ->
                let fold = line
                    |> String.trim
                    |> String.split_on_char ' '
                    |> (fun ls -> List.nth ls 2)
                    |> String.split_on_char '='
                in
                let fold_dim = (List.nth fold 0).[0] in
                let fold_line = int_of_string (List.nth fold 1) in
                load_folds ((fold_dim, fold_line) :: folds)
        | None -> folds
    in
    let folds = List.rev (load_folds []) in
    (points, folds)
;;


let fold_x x (dots : Point.t list) =
    let rec helper acc dots =
        match dots with
        | [] -> acc
        | hd :: rst ->
                let point' = if hd.x > x then Point.make (x - (hd.x - x)) hd.y else hd in
                helper (point' :: acc) rst
    in
    helper [] dots
;;


let fold_y y dots =
    let rec helper acc dots =
        match dots with
        | [] -> acc
        | hd :: rst ->
                let point' = if hd.y > y then Point.make hd.x (y - (hd.y - y)) else hd in
                helper (point' :: acc) rst
    in
    helper [] dots
;;


(* We could do this faster if the points were sorted, but I guess we would have to resort after
 * every fold?
 *
 * We could also remove redundant points after each step, which will save time (and basically
 * involve sorting anyway).
 * *)


let do_game points folds =
    let rec loop points folds =
        match folds with
        | [] ->
                List.sort_uniq Point.compare points
        | hd :: rst ->
                let points' =
                    let dim, line = hd in
                    Printf.printf "fold=%c %d\n" dim line;
                    let fold_f = if dim = 'x' then fold_x else fold_y in
                    fold_f line points
                in
                loop points' rst
    in
    loop points folds
;;


let process_file filename =
    let inc = open_in filename in
    Printf.printf "filename=%s\n" filename;
    let points, folds = load_file inc in
    let result = do_game points folds in

    Printf.printf "result=%s\n" (format_list result Point.fmt);
    Printf.printf "result=%d\n" (List.length result);

    let image = fill_matrix result in
    Printf.printf "image=%s\n" (format_matrix_of_int image);

    Printf.printf "\n";
;;


let () =
    process_file "13-test.input";
    process_file "13.input";
    (*
    *)
;;
