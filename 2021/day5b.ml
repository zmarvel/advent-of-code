(*
--- Part Two ---

Unfortunately, considering only horizontal and vertical lines doesn't give you the full picture; you need to also consider diagonal lines.

Because of the limits of the hydrothermal vent mapping system, the lines in your list will only ever be horizontal, vertical, or a diagonal line at exactly 45 degrees. In other words:

    An entry like 1,1 -> 3,3 covers points 1,1, 2,2, and 3,3.
    An entry like 9,7 -> 7,9 covers points 9,7, 8,8, and 7,9.

Considering all lines from the above example would now produce the following diagram:

1.1....11.
.111...2..
..2.1.111.
...1.2.2..
.112313211
...1.2....
..1...1...
.1.....1..
1.......1.
222111....

You still need to determine the number of points where at least two lines overlap. In the above example, this is still anywhere in the diagram with a 2 or larger - now a total of 12 points.

Consider all of the lines. At how many points do at least two lines overlap?
*)


open Utils;;


(** Load a list of 4-tuples of the form (x1, y1, x2, y2)
 *)
let load_file inc =
    let rec helper ls max_point =
        match input_line_opt inc with
        | Some(line) ->
            let parts = String.split_on_char ' ' line in
            (* Layout is [ "x1,y1"; "->"; "x2,y2" ] *)
            Printf.printf "%s\n" (format_list_of_string parts);

            let s1 = List.hd parts in
            let s2 = List.nth parts 2 in
            let convert_pos_string s =
                s
                |> String.split_on_char ','
                |> (fun ls ->
                        let x = int_of_string (List.hd ls) in
                        let y = int_of_string (List.hd (List.tl ls)) in
                        Point.make x y)
            in
            let p1 = convert_pos_string s1 in
            let p2 = convert_pos_string s2 in
            let max_point' =
                let open Point in
                let {x = x1; y = y1} = p1 in
                let {x = x2; y = y2} = p2 in
                let {x = cx; y = cy} = max_point in
                {x = max (max x1 x2) cx; y = max (max y1 y2) cy}
            in
            helper ((p1, p2) :: ls) max_point'
        | None -> ls, max_point
    in
    helper [] (Point.make 0 0)
;;



(** Run the simulated game, iterating through all the lines until they've all been drawn. Then,
    count all the cells that are > 1.
 *)
let do_game lines width height =
    (* This mask is 0 when a cell is covered *)
    let board = Array.make_matrix width height 0 in
    let draw_line line =
        let p1, p2 = line in
        let line_points = Point.points_in_line p1 p2 in
        List.iter (fun p ->
            let open Point in
            (* Printf.printf "%s\n" (Point.fmt p); *)
            let curr = board.(p.x).(p.y) in
            board.(p.x).(p.y) <- (succ curr)
        ) line_points
    in
    let rec line_loop lines =
        match lines with
        | line :: rest ->
                draw_line line;
                line_loop rest
        | [] -> board
    in
    let final_board = line_loop lines in
    Printf.printf "final_board=\n%s\n" (format_matrix_of_int final_board);
    let mask = matrix_greater_than 1 final_board in
    (* let masked_board = matrix_multiply final_board mask in *)
    let sum = matrix_sum mask in
    sum
;;


let process_file filename =
    Printf.printf "filename=%s\n" filename;
    let inc = open_in filename in
    let lines, max_point = load_file inc in
    let open Point in
    let {x = max_x; y = max_y} = max_point in
    let width = max_x + 1 in
    let height = max_y + 1 in
    Printf.printf "width=%d; height=%d\n" width height;
    let answer = do_game lines width height in

    Printf.printf "answer=%d\n" answer;
    Printf.printf "\n";
;;


let () =
    process_file "5-test.input";
    process_file "5.input";
;;
