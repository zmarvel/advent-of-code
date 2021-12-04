(*
--- Day 4: Giant Squid ---

You're already almost 1.5km (almost a mile) below the surface of the ocean, already so deep that you can't see any sunlight. What you can see, however, is a giant squid that has attached itself to the outside of your submarine.

Maybe it wants to play bingo?

Bingo is played on a set of boards each consisting of a 5x5 grid of numbers. Numbers are chosen at random, and the chosen number is marked on all boards on which it appears. (Numbers may not appear on all boards.) If all numbers in any row or any column of a board are marked, that board wins. (Diagonals don't count.)

The submarine has a bingo subsystem to help passengers (currently, you and the giant squid) pass the time. It automatically generates a random order in which to draw numbers and a random set of boards (your puzzle input). For example:

7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7

After the first five numbers are drawn (7, 4, 9, 5, and 11), there are no winners, but the boards are marked as follows (shown here adjacent to each other to save space):

22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
 8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
 6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
 1 12 20 15 19        14 21 16 12  6         2  0 12  3  7

After the next six numbers are drawn (17, 23, 2, 0, 14, and 21), there are still no winners:

22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
 8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
 6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
 1 12 20 15 19        14 21 16 12  6         2  0 12  3  7

Finally, 24 is drawn:

22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
 8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
 6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
 1 12 20 15 19        14 21 16 12  6         2  0 12  3  7

At this point, the third board wins because it has at least one complete row or column of marked numbers (in this case, the entire top row is marked: 14 21 17 24 4).

The score of the winning board can now be calculated. Start by finding the sum of all unmarked numbers on that board; in this case, the sum is 188. Then, multiply that sum by the number that was just called when the board won, 24, to get the final score, 188 * 24 = 4512.

To guarantee victory against the giant squid, figure out which board will win first. What will your final score be if you choose that board?
*)


open Utils;;


(** Load an list of numbers and a list of 5x5 boards from the file.
 *)
let load_file inc =
    (* First, read the list of numbers *)
    let nums =
        Option.map (function line ->
                String.split_on_char ',' line
                |> List.map int_of_string
                |> Array.of_list)
            (input_line_opt inc)
        |> Option.get
    in
    let rec read_board i board =
        if i = 5 then
            reverse_array (Array.of_list board)
        else
            let line =
                input_line inc
                |> String.trim
            in
            let row =
                String.split_on_char ' ' line
                |> List.filter (function s -> String.length s <> 0)
                |> Array.of_list
                |> Array.map int_of_string
            in
            read_board (succ i) (row :: board)
        in
    let rec read_boards boards =
        (* If the first line read is not empty, we're at EOF *)
        match input_line_opt inc with
        | Some(_) -> 
                read_boards ((read_board 0 []) :: boards)
        | None ->
                Array.of_list boards
    in
    (nums, read_boards [])
;;


(**
 * Return true when [mask] contains either a row or a column of all 0.
 * When a cell is 0, it has been "covered."
 *)
let is_winning_mask (mask : int array array) =
    let multiply_rows m =
        Array.fold_left (fun acc -> fun row -> multiply_array acc row) (Array.make 5 1) m
    in
    (* Multiply adjacent rows. If the resulting array is all 0, the board is winning *)
    let row_result = multiply_rows mask in
    (array_sum row_result = 0) || (
        (* Do the same for columns *)
        let mask_t = matrix_transpose mask in
        let col_result = multiply_rows mask_t in
        array_sum col_result = 0
    )
;;


(** Return a mask [m] where [m.(i).(j)] is 0 when [board.(i).(j) = num] and otherwise 1 *)
let mask_num_in_board num board =
    let dim1, dim2 = get_matrix_dims board in
    init_matrix dim1 dim2 (fun i -> fun j ->
        if board.(i).(j) = num then 0 else 1)
;;


(** Run the simulated game, iterating through nums, until a winning board is found, returning the winning number and the (masked) winning board.
 *)
let do_game nums boards =
    (* This mask is 0 when a cell is covered *)
    let covered_mask =
        Array.init (Array.length boards) (function _ -> Array.make_matrix 5 5 1)
    in
    let rec num_loop inum =
        if inum = Array.length nums then
            0, Array.make_matrix 5 5 0
        else
            let num = nums.(inum) in
            let rec board_loop iboard =
                (* Printf.printf "iboard=%d\n" iboard; *)
                if iboard = Array.length boards then
                    (* We got to the end without finding a winner *)
                    None
                else
                    (* Update all the boards--or at least their masks *)
                    (* This won't be very fast (searching each board for num) but that might be okay *)
                    let board = boards.(iboard) in
                    let board_mask = covered_mask.(iboard) in
                    let board_mask' = matrix_multiply board_mask (mask_num_in_board num board) in
                    Printf.printf "mask=";
                    print_matrix board_mask';
                    Printf.printf "\n";
                    Printf.printf "board=";
                    print_matrix board;
                    Printf.printf "\n";
                    if is_winning_mask board_mask' then (
                        let masked_board = matrix_multiply board board_mask' in
                        Printf.printf "masked_board=";
                        print_matrix masked_board;
                        Printf.printf "\n----------------------------------------------------------------------\n";
                        Some(masked_board))
                    else begin
                        covered_mask.(iboard) <- board_mask';
                        board_loop (succ iboard)
                    end
            in
            match board_loop 0 with
            | Some(winner) -> (num, winner)
            | None -> num_loop (succ inum)
    in
    num_loop 0
;;


let process_file filename =
    Printf.printf "filename=%s\n" filename;
    let inc = open_in filename in
    let nums, boards = load_file inc in
    let num, winner = do_game nums boards in

    Printf.printf "num=%d\nwinner=" num;
    print_matrix winner;

    Printf.printf "\n";
;;


let () =
    process_file "4-test.input";
    (* process_file "4.input"; *)
;;
