(*
--- Day 13: Transparent Origami ---
You reach another volcanically active part of the cave. It would be nice if you could do some kind of thermal imaging so you could tell ahead of time which caves are too hot to safely enter.

Fortunately, the submarine seems to be equipped with a thermal camera! When you activate it, you are greeted with:

Congratulations on your purchase! To activate this infrared thermal imaging
camera system, please enter the code found on page 1 of the manual.
Apparently, the Elves have never used this feature. To your surprise, you manage to find the manual; as you go to open it, page 1 falls out. It's a large sheet of transparent paper! The transparent paper is marked with random dots and includes instructions on how to fold it up (your puzzle input). For example:

6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5
The first section is a list of dots on the transparent paper. 0,0 represents the top-left coordinate. The first value, x, increases to the right. The second value, y, increases downward. So, the coordinate 3,0 is to the right of 0,0, and the coordinate 0,7 is below 0,0. The coordinates in this example form the following pattern, where # is a dot on the paper and . is an empty, unmarked position:

...#..#..#.
....#......
...........
#..........
...#....#.#
...........
...........
...........
...........
...........
.#....#.##.
....#......
......#...#
#..........
#.#........
Then, there is a list of fold instructions. Each instruction indicates a line on the transparent paper and wants you to fold the paper up (for horizontal y=... lines) or left (for vertical x=... lines). In this example, the first fold instruction is fold along y=7, which designates the line formed by all of the positions where y is 7 (marked here with -):

...#..#..#.
....#......
...........
#..........
...#....#.#
...........
...........
-----------
...........
...........
.#....#.##.
....#......
......#...#
#..........
#.#........
Because this is a horizontal line, fold the bottom half up. Some of the dots might end up overlapping after the fold is complete, but dots will never appear exactly on a fold line. The result of doing this fold looks like this:

#.##..#..#.
#...#......
......#...#
#...#......
.#.#..#.###
...........
...........
Now, only 17 dots are visible.

Notice, for example, the two dots in the bottom left corner before the transparent paper is folded; after the fold is complete, those dots appear in the top left corner (at 0,0 and 0,1). Because the paper is transparent, the dot just below them in the result (at 0,3) remains visible, as it can be seen through the transparent paper.

Also notice that some dots can end up overlapping; in this case, the dots merge together and become a single dot.

The second fold instruction is fold along x=5, which indicates this line:

#.##.|#..#.
#...#|.....
.....|#...#
#...#|.....
.#.#.|#.###
.....|.....
.....|.....
Because this is a vertical line, fold left:

#####
#...#
#...#
#...#
#####
.....
.....
The instructions made a square!

The transparent paper is pretty big, so for now, focus on just completing the first fold. After the first fold in the example above, 17 dots are visible - dots that end up overlapping after the fold is completed count as a single dot.

How many dots are visible after completing just the first fold instruction on your transparent paper?
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
    let result = do_game points [(List.hd folds)] in

    Printf.printf "result=%s\n" (format_list result Point.fmt);
    Printf.printf "result=%d\n" (List.length result);

    Printf.printf "\n";
;;

let () =
    process_file "13-test.input";
    process_file "13.input";
    (*
    *)
;;
