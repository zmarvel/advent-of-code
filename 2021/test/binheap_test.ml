
open OUnit2;;


let test_insert _ =
    let heap = Binheap.make 16 in
    Binheap.insert heap 1 3;
    assert_equal (Binheap.capacity heap) 16;
    assert_equal (Binheap.size heap) 1;
    (* Didn't test that we actually put a "1" in the array, but we'll test that when we go to remove
     * it
     *)
    Binheap.insert heap 2 3;
    assert_equal (Binheap.capacity heap) 16;
    assert_equal (Binheap.size heap) 2;
;;


let test_extract _ =
    let heap = Binheap.make 16 in

    Binheap.insert heap 1 3;
    Binheap.insert heap 2 3;
    assert_equal (Binheap.extract heap) (1, 3);
    assert_equal (Binheap.extract heap) (2, 3);
    assert_equal (Binheap.empty heap) true;

    Binheap.insert heap 2 3;
    Binheap.insert heap 2 3;
    Binheap.insert heap 1 3;
    assert_equal (Binheap.size heap) 3;
    assert_equal (Binheap.extract heap) (1, 3);
    assert_equal (Binheap.extract heap) (2, 3);
    assert_equal (Binheap.extract heap) (2, 3);
    assert_equal (Binheap.empty heap) true;
;;


let string_of_int_pair p =
    let a, b = p in
    Printf.sprintf "(%d, %d)" a b
;;


let test_make_from _ =
    let a = [| (8, 3); (5, 3); (3, 3); (4, 3); (11, 3); |] in
    let heap = Binheap.make_from 16 a in
    assert_equal (Binheap.size heap) 5;
    assert_equal (3, 3) (Binheap.extract heap) ~printer:string_of_int_pair;
    assert_equal (4, 3) (Binheap.extract heap) ~printer:string_of_int_pair;
    assert_equal (5, 3) (Binheap.extract heap) ~printer:string_of_int_pair;
    assert_equal (8, 3) (Binheap.extract heap) ~printer:string_of_int_pair;
    assert_equal (11, 3) (Binheap.extract heap) ~printer:string_of_int_pair;

    let a = [| (11, 5); (4, 5); (3, 5); (5, 5); (8, 5); |] in
    let heap = Binheap.make_from 16 a in
    assert_equal (Binheap.size heap) 5;
    assert_equal (3, 5) (Binheap.extract heap) ~printer:string_of_int_pair;
    assert_equal (4, 5) (Binheap.extract heap) ~printer:string_of_int_pair;
    assert_equal (5, 5) (Binheap.extract heap) ~printer:string_of_int_pair;
    assert_equal (8, 5) (Binheap.extract heap) ~printer:string_of_int_pair;
    assert_equal (11, 5) (Binheap.extract heap) ~printer:string_of_int_pair;
;;


let suite = "Test AOC Binheap" >::: [
    "test_insert" >:: test_insert ;
    "test_extract" >:: test_extract ;
    "test_make_from" >:: test_make_from ;
];;


let () =
    run_test_tt_main suite
;;
