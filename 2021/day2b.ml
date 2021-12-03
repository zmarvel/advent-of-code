(*
 * --- Part Two ---
 *
 * Based on your calculations, the planned course doesn't seem to make any sense. You find the submarine manual and discover that the process is actually slightly more complicated.
 *
 * In addition to horizontal position and depth, you'll also need to track a third value, aim, which also starts at 0. The commands also mean something entirely different than you first thought:
 *
 *     down X increases your aim by X units.
 *     up X decreases your aim by X units.
 *     forward X does two things:
 *         It increases your horizontal position by X units.
 *         It increases your depth by your aim multiplied by X.
 *
 * Again note that since you're on a submarine, down and up do the opposite of what you might expect: "down" means aiming in the positive direction.
 *
 * Now, the above example does something different:
 *
 *     forward 5 adds 5 to your horizontal position, a total of 5. Because your aim is 0, your depth does not change.
 *     down 5 adds 5 to your aim, resulting in a value of 5.
 *     forward 8 adds 8 to your horizontal position, a total of 13. Because your aim is 5, your depth increases by 8*5=40.
 *     up 3 decreases your aim by 3, resulting in a value of 2.
 *     down 8 adds 8 to your aim, resulting in a value of 10.
 *     forward 2 adds 2 to your horizontal position, a total of 15. Because your aim is 10, your depth increases by 2*10=20 to a total of 60.
 *
 * After following these new instructions, you would have a horizontal position of 15 and a depth of 60. (Multiplying these produces 900.)
 *
 * Using this new interpretation of the commands, calculate the horizontal position and depth you would have after following the planned course. What do you get if you multiply your final horizontal position by your final depth?
 *)


let input_line_opt inc =
    try
        let line = input_line inc in
        Some(line)
    with
    | End_of_file -> None
;;


exception Invalid_direction of string


(* In addition to representing an absolute position as in day2a, the tuple will also now represent a
   position and an "aim"--it will become a 3-tuple of (horizontal position, depth, aim)
   *)


let parse_direction dirstr =
    match dirstr with
    | "forward" -> (1, 0, 0)
    | "down" -> (0, 0, 1)
    | "up" -> (0, 0, -1)
    | _ -> raise (Invalid_direction dirstr)
;;


let mult_position p1 p2 =
    let x1, y1, z1 = p1 in
    let x2, y2, z2 = p2 in
    (x1 * x2, y1 * y2, z1 * z2)
;;


let parse_step line =
    let line = String.trim line in
    let parts = String.split_on_char ' ' line in
    let direction = parse_direction (List.hd parts) in
    let magnitude = int_of_string (List.hd (List.tl parts)) in
    mult_position direction (magnitude, magnitude, magnitude)
;;


(* p1 is current position, p2 is step
 *)
let step_position p1 p2 =
    let x1, y1, z1 = p1 in
    let x2, y2, z2 = p2 in
    (x1 + x2, y1 + y2 + x2 * z1, z1 + z2)
;;


(* Let x = forward, y = up/down (depth)
 *)
let find_final_position inc =
    let rec helper pos =
        match (input_line_opt inc) with
        | Some(line) ->
                let step = parse_step line in
                let pos' = step_position pos step in
                helper pos'
        | None ->
                pos
    in
    helper (0, 0, 0)
;;


let process_file filename =
    Printf.printf "filename=%s\n" filename;
    let inc = open_in filename in
    let final_x, final_y, final_z = find_final_position inc in
    Printf.printf "final_position=(%d, %d, %d)\n" final_x final_y final_z;
    Printf.printf "result=%d\n" (final_x * final_y);
;;


let () =
    process_file "2-test.input";
    process_file "2.input";
;;
