

type 'a t = {
    a: 'a array;

    (* "Size" is how many nodes are in the tree. "Capacity" is how many nodes can be held without
       resizing the array, and doesn't have to be maintained explicitly since the array has a
       "length" property. *)
    mutable size: int;
};;


let make capacity =
    { a = Array.make capacity 0; size = 0 }
;;



let rec heapify (heap : 'a t) (i : int) =
    let left = 2 * i + 1 in
    let right = 2 * i + 2 in
    let smallest = i in
    let smallest =
        if left < heap.size && heap.a.(left) < heap.a.(smallest) then left
        else smallest
    in
    let smallest =
        if right < heap.size && (heap.a.(right) < heap.a.(smallest)) then right
        else smallest
    in
    (* Printf.printf "i=%d left=%d right=%d smallest=%d\n" i left right smallest; *)
    if smallest <> i then begin
        let tmp = heap.a.(smallest) in
        heap.a.(smallest) <- heap.a.(i);
        heap.a.(i) <- tmp;
        heapify heap smallest
    end
;;


let make_from capacity a =
    (* TODO: If capacity < Array.length a, raise *)
    let a' = Array.make capacity 0 in
    Array.blit a 0 a' 0 (Array.length a);
    let heap = { a = a'; size = Array.length a } in
    let rec loop i = 
        (*
        Printf.printf "[| ";
        Array.iter (Printf.printf "%d ") a';
        Printf.printf "|]\n";
        *)
        if i < 0 then heap
        else begin
            heapify heap i;
            loop (pred i)
        end
    in
    loop ((Array.length a / 2) - 1)
;;


let capacity heap =
    Array.length heap.a
;;


let size heap =
    heap.size
;;


let empty heap =
    heap.size = 0
;;


(* TODO: raise on empty, optional version *)
let extract heap =
    let iswap = heap.size - 1 in
    let root = heap.a.(0) in
    heap.size <- heap.size - 1;
    heap.a.(0) <- heap.a.(iswap);
    heapify heap 0;
    root
;;


let parent i =
    (i - 1) / 2
;;


let insert heap value =
    if (size heap) = (capacity heap) then (* TODO: Resize the array *) ();
    let asize = heap.size in
    heap.a.(asize) <- value;
    heap.size <- asize + 1;
    (* Printf.printf "size=%d size'=%d\n" asize heap.size; *)
    let rec loop i =
        if i = 0 then ()
        else
            let iparent = parent i in
            if heap.a.(i) < heap.a.(iparent)  then
            (* Swap the two nodes *)
            let tmp = heap.a.(i) in
            heap.a.(i) <- heap.a.(iparent);
            heap.a.(iparent) <- tmp;
            loop (iparent);
    in
    loop asize;
;;
