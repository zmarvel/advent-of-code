(*
 * --- Day 2: Dive! ---
 *
 * Now, you need to figure out how to pilot this thing.
 *
 * It seems like the submarine can take a series of commands like forward 1, down 2, or up 3:
 *
 *     forward X increases the horizontal position by X units.
 *     down X increases the depth by X units.
 *     up X decreases the depth by X units.
 *
 * Note that since you're on a submarine, down and up affect your depth, and so they have the opposite result of what you might expect.
 *
 * The submarine seems to already have a planned course (your puzzle input). You should probably figure out where it's going. For example:
 *
 * forward 5
 * down 5
 * forward 8
 * up 3
 * down 8
 * forward 2
 *
 * Your horizontal position and depth both start at 0. The steps above would then modify them as follows:
 *
 *     forward 5 adds 5 to your horizontal position, a total of 5.
 *     down 5 adds 5 to your depth, resulting in a value of 5.
 *     forward 8 adds 8 to your horizontal position, a total of 13.
 *     up 3 decreases your depth by 3, resulting in a value of 2.
 *     down 8 adds 8 to your depth, resulting in a value of 10.
 *     forward 2 adds 2 to your horizontal position, a total of 15.
 *
 * After following these instructions, you would have a horizontal position of 15 and a depth of 10. (Multiplying these together produces 150.)
 *
 * Calculate the horizontal position and depth you would have after following the planned course. What do you get if you multiply your final horizontal position by your final depth?
 *)


let input_line_opt inc =
    try
        let line = input_line inc in
        Some(line)
    with
    | End_of_file -> None
;;


exception Invalid_direction of string


let parse_direction dirstr =
    match dirstr with
    | "forward" -> (1, 0)
    | "down" -> (0, 1)
    | "up" -> (0, -1)
    | _ -> raise (Invalid_direction dirstr)
;;


let add_position p1 p2 =
    let x1, y1 = p1 in
    let x2, y2 = p2 in
    (x1 + x2, y1 + y2)
;;


let mult_position p1 p2 =
    let x1, y1 = p1 in
    let x2, y2 = p2 in
    (x1 * x2, y1 * y2)
;;


let parse_position line =
    let line = String.trim line in
    let parts = String.split_on_char ' ' line in
    let direction = parse_direction (List.hd parts) in
    let magnitude = int_of_string (List.hd (List.tl parts)) in
    mult_position direction (magnitude, magnitude)
;;


(* Let x = forward, y = up/down (depth)
 *)
let find_final_position inc =
    let rec helper pos =
        match (input_line_opt inc) with
        | Some(line) -> 
                let position_delta = parse_position line in
                let pos' = add_position pos position_delta in
                helper pos'
        | None ->
                pos
    in
    helper (0, 0)
;;


let process_file filename =
    Printf.printf "filename=%s\n" filename;
    let inc = open_in filename in
    let final_x, final_y = find_final_position inc in
    Printf.printf "final_position=(%d, %d)\n" final_x final_y;
    Printf.printf "result=%d\n" (final_x * final_y);
;;


let () =
    process_file "2-test.input";
    process_file "2.input";
;;
