

open OUnit2;;
open Utils;;


let test_ctoi _ =
    assert_equal (ctoi '0') 0;
    assert_equal (ctoi '9') 9;
    assert_raises (Invalid_digit 'a') (fun _ ->
        ctoi 'a');
;;


let test_array_add _ =
    let a1 = Array.init 10 (fun i -> 2 * i + 1) in
    let a2 = Array.init 10 (fun i -> 2 * i) in
    let result = array_add a1 a2 in
    assert_equal (array_ifor_all (fun i -> fun x -> x = 4 * i + 1) result) true;
;;


let test_array_sub _ =
    let a1 = Array.init 10 (fun i -> 2 * i + 1) in
    let a2 = Array.init 10 (fun i -> 2 * i) in
    let result = array_sub a1 a2 in
    assert_equal (array_ifor_all (fun _ -> fun x -> x = 1) result) true;
;;


let test_array_multiply _ =
    let a1 = Array.make 6 0 in
    let a2 = Array.make 6 1 in
    let result = array_multiply a1 a2 in
    assert_equal (Array.length result) 6;
    assert_equal result a1;
    assert_equal (array_multiply a1 a2) (array_multiply a2 a1);
;;


let test_array_slide_left _ =
    let a = Array.init 10 (fun i -> i) in
    let a' = array_slide_left 2 (-1) a in
    assert_equal (Array.length a) (Array.length a');
    assert_equal a'.(8) (-1);
    assert_equal a'.(9) (-1);
    assert_equal (Array.sub a' 0 8) (Array.sub a 2 8);
;;


let test_array_slide_right _ =
    let a = Array.init 10 (fun i -> i) in
    let a' = array_slide_right 2 (-1) a in
    assert_equal (Array.length a) (Array.length a');
    assert_equal a'.(0) (-1);
    assert_equal a'.(1) (-1);
    assert_equal (Array.sub a' 2 8) (Array.sub a 0 8);
;;


let test_array_abs _ =
    let a = Array.init 6 (fun i -> i) in
    assert_equal (array_abs a) a;
    let a_negative = Array.init 6 (fun i -> (-i)) in
    assert_equal (array_abs a_negative) a;
;;


let test_array_add_scalar _ =
    let a = Array.make 6 1 in
    assert_equal (array_add_scalar a 3) (Array.make 6 4);
    assert_equal (array_add_scalar a (-3)) (Array.make 6 (-2));
;;


let test_array_sub_scalar _ =
    let a = Array.make 6 1 in
    assert_equal (array_sub_scalar a 2) (Array.make 6 (-1));
    let a = Array.make 6 5 in
    assert_equal (array_sub_scalar a 2) (Array.make 6 3);
;;


let test_array_sum _ =
    let a = Array.make 6 0 in
    let a = Array.mapi (fun i -> fun _ -> i) a in
    assert_equal (array_sum a) (1 + 2 + 3 + 4 + 5);
;;


let test_array_iota _ =
    assert_raises (Invalid_argument "array_iota") (fun _ ->
        array_iota 5 4
    );
    assert_equal (array_iota 4 5) (Array.make 1 4);
    assert_equal (array_iota (-4) 4) (array_sub (Array.init 8 (fun i -> i)) (Array.make 8 4));
;;


let test_array_reverse _ =
    let a = array_iota 4 9 in
    let a' = array_reverse a in
    assert_equal a'.(0) a.(4);
    assert_equal a'.(4) a.(0);
;;



let test_matrix_transpose _ =
    let m = [| [| 1; 2; 3; 4; 5 |] ;
               [| 6; 7; 8; 9; 10 |] ; |] in
    let expected = [|
        [| 1; 6 |] ;
        [| 2; 7 |] ;
        [| 3; 8 |] ;
        [| 4; 9 |] ;
        [| 5; 10 |] ; |]
    in
    let result = matrix_transpose m in
    assert_equal result expected;
;;


let test_matrix_multiply _ =
    let m1 = Array.make_matrix 3 5 1 in
    let m2 = Array.make_matrix 3 5 0 in
    let result = matrix_multiply m1 m2 in
    assert_equal (get_matrix_dims result) (3, 5);
    assert_equal result (Array.make_matrix 3 5 0);
    assert_equal result (matrix_multiply m2 m1);

    m1.(1).(1) <- 2;
    let result = matrix_multiply m2 m1 in
    assert_equal (get_matrix_dims result) (3, 5);
    assert_equal result m2;
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



let suite = "Test AOC Utils" >::: [
    "test_ctoi" >:: test_ctoi ;

    "test_array_add" >:: test_array_add ;
    "test_array_sub" >:: test_array_sub ;
    "test_array_multiply" >:: test_array_multiply ;
    "test_array_slide_left" >:: test_array_slide_left ;
    "test_array_slide_right" >:: test_array_slide_right ;
    "test_array_abs" >:: test_array_abs ;
    "test_array_add_scalar" >:: test_array_add_scalar ;
    "test_array_sub_scalar" >:: test_array_sub_scalar ;
    "test_array_sum" >:: test_array_sum ;
    "test_array_iota" >:: test_array_iota;
    "test_array_reverse" >:: test_array_reverse ;

    "test_matrix_multiply" >:: test_matrix_multiply ;
    "test_matrix_transpose" >:: test_matrix_transpose ;
];;

let () =
    run_test_tt_main suite
;;
