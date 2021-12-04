

open OUnit2;;
open Utils;;


let test_matrix_multiply _ =
    let m1 = Array.make_matrix 3 5 1 in
    let m2 = Array.make_matrix 3 5 0 in
    let result = matrix_multiply m1 m2 in
    assert_equal (get_matrix_dims result) (3, 5);
    assert_equal result (Array.make_matrix 3 5 0);

    let result = matrix_multiply m2 m1 in
    assert_equal (get_matrix_dims result) (3, 5);
    assert_equal result (Array.make_matrix 3 5 0);

    m1.(1).(1) <- 2;
    let result = matrix_multiply m2 m1 in
    assert_equal (get_matrix_dims result) (3, 5);
    assert_equal result m2;
;;

let suite = "Test AOC Utils" >::: [
    "test_matrix_multiply" >:: test_matrix_multiply ;
];;

let () =
    run_test_tt_main suite
;;
