

type 'a t = {
    a: 'a array;

    (* "Size" is how many nodes are in the tree. "Capacity" is how many nodes can be held without
       resizing the array, and doesn't have to be maintained explicitly since the array has a
       "length" property. *)
    size: int;

    (* Store compare func here? *)
    compare: ('a -> 'a -> int);
};;


let make capacity compare =
    { a = Array.make capacity 0; size = 0; compare }
;;


let capacity heap =
    Array.length heap.a
;;


let size heap =
    heap.size
;;


let rec heapify (heap : 'a t) (i : int) =
    let left = 2 * i in
    let right = 2 * i + 1 in
    let smallest = i in
    let smallest =
        if left < heap.size && heap.compare heap.a.(left) heap.a.(smallest) = -1 then left
        else smallest
    in
    let smallest =
        if right < heap.size && (heap.compare heap.a.(right) heap.a.(smallest) = -1) then right
        else smallest
    in
    if smallest <> i then begin
        let tmp = heap.a.(smallest) in
        heap.a.(smallest) <- heap.a.(i);
        heap.a.(i) <- tmp;
        heapify heap smallest
    end
;;


(* TODO: raise on empty *)
let extract heap =
    let iswap = heap.size - 1 in
    let root = heap.a.(0) in
    let rsize = ref heap.size in
    rsize := !rsize - 1;
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
    (* TODO increase size *)
    let rsize = ref heap.size in
    rsize := asize + 1;
    let rec loop i =
        if i = 0 then ()
        else
            let iparent = parent i in
            if heap.compare heap.a.(i) heap.a.(iparent) = -1 then
            (* Swap the two nodes *)
            let tmp = heap.a.(i) in
            heap.a.(i) <- heap.a.(iparent);
            heap.a.(iparent) <- tmp;
            loop (iparent);
    in
    loop asize;
;;
