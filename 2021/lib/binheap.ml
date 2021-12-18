

type 'a t = {
    a: 'a node array;

    (* "Size" is how many nodes are in the tree. "Capacity" is how many nodes can be held without
       resizing the array, and doesn't have to be maintained explicitly since the array has a
       "length" property. *)
    mutable size: int;
}

and 'a node =
    Empty
    | Node of (int * 'a) (* Node of (priority, key) -- should I use a record? *)
;;


let make capacity =
    { a = Array.make capacity Empty; size = 0 }
;;


exception Empty_binheap


let get_priority heap i =
    match heap.a.(i) with
    | Node(prio, _) -> prio
    | Empty -> raise Empty_binheap
;;


let get_key heap i =
    match heap.a.(i) with
    | Node(_, key) -> key
    | Empty -> raise Empty_binheap
;;


let rec heapify (heap : 'a t) (i : int) =
    let left = 2 * i + 1 in
    let right = 2 * i + 2 in
    let smallest = i in
    let currprio = get_priority heap smallest in
    let smallest =
        if left < heap.size then
            let leftprio = get_priority heap left in
            if leftprio < currprio then left else smallest
        else smallest
    in
    let currprio = get_priority heap smallest in
    let smallest =
        if right < heap.size then
            let rightprio = get_priority heap right in
            if rightprio < currprio then right else smallest
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


let make_from capacity (a : (int * 'a) array) : 'a t =
    (* TODO: If capacity < Array.length a, raise *)
    let a' = Array.make capacity Empty in
    Array.iteri (fun i -> fun x ->
        let prio, key = x in
        a'.(i) <- Node(prio, key)
    ) a;
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
    (* Remove the root node, swap it with the right-most leaf, and re-heapify *)
    let iswap = heap.size - 1 in
    let root = heap.a.(0) in
    heap.size <- heap.size - 1;
    heap.a.(0) <- heap.a.(iswap);
    heapify heap 0;
    match root with
    | Node(priority, key) -> (priority, key)
    | Empty -> raise Empty_binheap
;;


let parent i =
    (i - 1) / 2
;;


let insert heap key priority =
    if (size heap) = (capacity heap) then (* TODO: Resize the array *) ();
    let asize = heap.size in
    heap.a.(asize) <- Node(priority, key);
    heap.size <- asize + 1;
    (* Printf.printf "size=%d size'=%d\n" asize heap.size; *)
    (* Recursively walk up the tree until the heap property is restored *)
    let rec loop i =
        if i = 0 then ()
        else
            let iparent = parent i in
            let curr_prio = get_priority heap i in
            let parent_prio = get_priority heap iparent in
            if curr_prio < parent_prio then
            (* Swap the two nodes *)
            let tmp = heap.a.(i) in
            heap.a.(i) <- heap.a.(iparent);
            heap.a.(iparent) <- tmp;
            loop iparent;
    in
    loop asize
;;


let decrease_key heap key priority =
    (* We have a min heap, so heapify upwards recursively *)
    (* Just linearly search for the key. TODO: Could we be more clever by searching by level? *)
    let rec find_key i =
        (* TODO raise if key not found (check heap.size) *)
        if get_key heap i = key then i
        else find_key (succ i)
    in
    let key_pos = find_key 0 in
    heap.a.(key_pos) <- Node(key, priority);

    let rec heapify_loop i =
        if i = 0 then ()
        else
            let iparent = parent i in
            let curr_prio = get_priority heap i in
            let parent_prio = get_priority heap iparent in
            if curr_prio < parent_prio then
                let tmp = heap.a.(i) in
                heap.a.(i) <- heap.a.(iparent);
                heap.a.(iparent) <- tmp;
                heapify_loop iparent;
    in
    heapify_loop key_pos
;;
