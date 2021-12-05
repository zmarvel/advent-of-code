(*
--- Part Two ---

On the other hand, it might be wise to try a different strategy: let the giant squid win.

You aren't sure how many bingo boards a giant squid could play at once, so rather than waste time counting its arms, the safe thing to do is to figure out which board will win last and choose that one. That way, no matter which boards it picks, it will win for sure.

In the above example, the second board is the last to win, which happens after 13 is eventually called and its middle column is completely marked. If you were to keep playing until this point, the second board would have a sum of unmarked numbers equal to 148 for a final score of 148 * 13 = 1924.

Figure out which board will win last. Once it wins, what would its final score be?
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
    let sum_rows m =
        Array.map (fun row -> array_sum row) m
    in
    let exists_zero a =
        Array.mem 0 a
    in
    (* Add each row. If any of the results are 0, the board is winning. *)
    let row_result = sum_rows mask in
    (exists_zero row_result) || (
        (* Do the same for columns *)
        let mask_t = matrix_transpose mask in
        let col_result = sum_rows mask_t in
        exists_zero col_result
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
    let board_has_won =
        Array.make (Array.length boards) false
    in
    let rec num_loop inum winner_list =
        if inum = Array.length nums then
            winner_list
        else
            let num = nums.(inum) in
            let rec board_loop iboard winner_list =
                (* Printf.printf "iboard=%d\n" iboard; *)
                if iboard = Array.length boards then
                    (* We got to the end without finding a winner *)
                    winner_list
                else
                    (* Update all the boards--or at least their masks *)
                    (* This won't be very fast (searching each board for num) but that might be okay *)
                    let board = boards.(iboard) in
                    let board_mask = covered_mask.(iboard) in
                    let board_mask' = matrix_multiply board_mask (mask_num_in_board num board) in
                    (*
                    Printf.printf "mask=";
                    print_matrix board_mask';
                    Printf.printf "\n";
                    Printf.printf "board=";
                    print_matrix board;
                    Printf.printf "\n";
                    *)
                    covered_mask.(iboard) <- board_mask';
                    if not board_has_won.(iboard) && is_winning_mask board_mask' then
                        let masked_board = matrix_multiply board board_mask' in
                        (*
                        Printf.printf "masked_board=";
                        print_matrix masked_board;
                        Printf.printf "\n----------------------------------------------------------------------\n";
                        *)
                        board_has_won.(iboard) <- true;
                        board_loop (succ iboard) ((num, masked_board) :: winner_list)
                    else
                        board_loop (succ iboard) winner_list
            in
            let winners = Array.of_list (board_loop 0 []) in
            num_loop (succ inum) (Array.append winner_list winners)
    in
    let winners = num_loop 0 (Array.init 0 (fun _ -> (0, Array.make_matrix 5 5 1))) in

    (* let print_winner_list = 
        Array.iter (fun p -> let _, m = p in print_matrix m; Printf.printf "\n";) winners;
    in
    print_winner_list; *)

    let num, last_winner = winners.(Array.length winners - 1) in
    let winner_sum = Array.fold_left (fun acc -> fun row -> acc + array_sum row) 0 last_winner in
    let final_answer = winner_sum * num in
    final_answer
;;


let process_file filename =
    Printf.printf "filename=%s\n" filename;
    let inc = open_in filename in
    let nums, boards = load_file inc in
    let answer = do_game nums boards in

    Printf.printf "answer=%d\n" answer;
    Printf.printf "\n";
;;


let () =
    process_file "4-test.input";
    process_file "4.input";
;;
