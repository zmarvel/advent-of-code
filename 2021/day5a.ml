(*
--- Day 5: Hydrothermal Venture ---

You come across a field of hydrothermal vents on the ocean floor! These vents constantly produce large, opaque clouds, so it would be best to avoid them if possible.

They tend to form in lines; the submarine helpfully produces a list of nearby lines of vents (your puzzle input) for you to review. For example:

0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2

Each line of vents is given as a line segment in the format x1,y1 -> x2,y2 where x1,y1 are the coordinates of one end the line segment and x2,y2 are the coordinates of the other end. These line segments include the points at both ends. In other words:

    An entry like 1,1 -> 1,3 covers points 1,1, 1,2, and 1,3.
    An entry like 9,7 -> 7,7 covers points 9,7, 8,7, and 7,7.

For now, only consider horizontal and vertical lines: lines where either x1 = x2 or y1 = y2.

So, the horizontal and vertical lines from the above list would produce the following diagram:

.......1..
..1....1..
..1....1..
.......1..
.112111211
..........
..........
..........
..........
222111....

In this diagram, the top left corner is 0,0 and the bottom right corner is 9,9. Each position is shown as the number of lines which cover that point or . if no line covers that point. The top-left pair of 1s, for example, comes from 2,2 -> 2,1; the very bottom row is formed by the overlapping lines 0,9 -> 5,9 and 0,9 -> 2,9.

To avoid the most dangerous areas, you need to determine the number of points where at least two lines overlap. In the above example, this is anywhere in the diagram with a 2 or larger - a total of 5 points.

Consider only horizontal and vertical lines. At how many points do at least two lines overlap?
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
                let p1, p2 = line in
                let open Point in
                let line_dir = get_direction p1 p2 in
                let {x = xdir; y = ydir} = line_dir in
                if xdir = 0 || ydir = 0 then
                    draw_line line;
                line_loop rest
        | [] -> board
    in
    let final_board = line_loop lines in
    Printf.printf "final_board=\n%s\n" (format_matrix_of_int final_board);
    let mask = matrix_greater_than final_board 1 in
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
