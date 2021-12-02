(*
 * --- Part Two ---
 * Considering every single measurement isn't as useful as you expected: there's just too much noise
 * in the data.
 *
 * Instead, consider sums of a three-measurement sliding window. Again considering the above
 * example:
 *
 * 199  A      
 * 200  A B    
 * 208  A B C  
 * 210    B C D
 * 200  E   C D
 * 207  E F   D
 * 240  E F G  
 * 269    F G H
 * 260      G H
 * 263        H
 * Start by comparing the first and second three-measurement windows. The measurements in the
 * first window are marked A (199, 200, 208); their sum is 199 + 200 + 208 = 607. The second
 * window is marked B (200, 208, 210); its sum is 618. The sum of measurements in the second
 * window is larger than the sum of the first, so this first comparison increased.
 *
 * Your goal now is to count the number of times the sum of measurements in this sliding window
 * increases from the previous sum. So, compare A with B, then compare B with C, then C with D,
 * and so on. Stop when there aren't enough measurements left to create a new three-measurement
 * sum.
 *
 * In the above example, the sum of each three-measurement window is as follows:
 *
 * A: 607 (N/A - no previous sum)
 * B: 618 (increased)
 * C: 618 (no change)
 * D: 617 (decreased)
 * E: 647 (increased)
 * F: 716 (increased)
 * G: 769 (increased)
 * H: 792 (increased)
 * In this example, there are 5 sums that are larger than the previous sum.
 *
 * Consider sums of a three-measurement sliding window. How many sums are larger than the
 * previous sum?
 *)

let maybe_read_line in_channel =
    try
        Some(input_line in_channel)
    with
    | End_of_file -> None
    | e -> raise e
;;

let sum_list =
    List.fold_left (+) 0
;;

let sum_array =
    Array.fold_left (+) 0
;;

let sum_window win =
    let window_len = Array.length win - 1 in
    let win1 = Array.sub win 0 window_len in
    let win2 = Array.sub win 1 window_len in
    (sum_array win1, sum_array win2)
;;

let add_to_window win x =
    let len = Array.length win in
    let rec shuffle_left i =
        if i == (len - 1) then
            ()
        else begin
            let next = Array.get win (i + 1) in
            Array.set win i next;
                shuffle_left (i + 1);
        end
    in
    shuffle_left 0;
    Array.set win (len - 1) x;
;;

let print_window win =
    Array.iter (Printf.printf "%d ") win;
    Printf.printf "\n";
;;

let () =
    let inc = open_in "1a.input" in
    let rec helper count i window =
        match maybe_read_line inc with
        | Some(line) ->
                let num = int_of_string (String.trim line) in
                if i < 3 then begin
                    add_to_window window num;
                    helper count (i + 1) window
                end
                else begin
                    add_to_window window num;
                    let sum1, sum2 = sum_window window in
                    let count' = if sum2 > sum1 then count + 1 else count in
                    Printf.printf "i=%d sum1=%d sum2=%d\n" i sum1 sum2;
                    helper count' (i + 1) window
                end
        | None ->
                count
                    in
    let result = helper 0 0 (Array.make 4 0) in
    Printf.printf "%d\n" result
;;
