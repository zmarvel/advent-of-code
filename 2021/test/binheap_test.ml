
open OUnit2;;


let test_insert _ =
    let heap = Binheap.make 16 in
    Binheap.insert heap 1;
    assert_equal (Binheap.capacity heap) 16;
    assert_equal (Binheap.size heap) 1;
    (* Didn't test that we actually put a "1" in the array, but we'll test that when we go to remove
     * it
     *)
    Binheap.insert heap 2;
    assert_equal (Binheap.capacity heap) 16;
    assert_equal (Binheap.size heap) 2;
;;


let test_extract _ =
    let heap = Binheap.make 16 in

    Binheap.insert heap 1;
    Binheap.insert heap 2;
    assert_equal (Binheap.extract heap) 1;
    assert_equal (Binheap.extract heap) 2;
    assert_equal (Binheap.empty heap) true;

    Binheap.insert heap 2;
    Binheap.insert heap 2;
    Binheap.insert heap 1;
    assert_equal (Binheap.size heap) 3;
    assert_equal (Binheap.extract heap) 1;
    assert_equal (Binheap.extract heap) 2;
    assert_equal (Binheap.extract heap) 2;
    assert_equal (Binheap.empty heap) true;
;;


let test_make_from _ =
    let a = [| 8; 5; 3; 4; 11; |] in
    let heap = Binheap.make_from 16 a in
    assert_equal (Binheap.size heap) 5;
    assert_equal 3 (Binheap.extract heap) ~printer:string_of_int;
    assert_equal 4 (Binheap.extract heap) ~printer:string_of_int;
    assert_equal 5 (Binheap.extract heap) ~printer:string_of_int;
    assert_equal 8 (Binheap.extract heap) ~printer:string_of_int;
    assert_equal 11 (Binheap.extract heap) ~printer:string_of_int;

    let a = [| 11; 4; 3; 5; 8; |] in
    let heap = Binheap.make_from 16 a in
    assert_equal (Binheap.size heap) 5;
    assert_equal 3 (Binheap.extract heap) ~printer:string_of_int;
    assert_equal 4 (Binheap.extract heap) ~printer:string_of_int;
    assert_equal 5 (Binheap.extract heap) ~printer:string_of_int;
    assert_equal 8 (Binheap.extract heap) ~printer:string_of_int;
    assert_equal 11 (Binheap.extract heap) ~printer:string_of_int;
;;


let suite = "Test AOC Binheap" >::: [
    "test_insert" >:: test_insert ;
    "test_extract" >:: test_extract ;
    "test_make_from" >:: test_make_from ;
];;


let () =
    run_test_tt_main suite
;;
