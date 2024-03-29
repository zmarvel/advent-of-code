

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


let size heap =
    heap.size
;;


let get_key_opt heap i =
    if i < size heap then
        match heap.a.(i) with
        | Node(_, key) -> Some(key)
        | Empty -> None
    else None
;;

(** Get the index of the left child of node at index [i]. *)
let get_left_child i = 2 * i + 1 ;;

(** Get the index of the right child of node at index [i]. *)
let get_right_child i = 2 * i + 2;;

let rec heapify (heap : 'a t) (i : int) =
    let left = get_left_child i in
    let right = get_right_child i in
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


let insert heap priority key =
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

exception Key_not_found of int

let decrease_key heap priority key =
    (* We have a min heap, so heapify upwards recursively *)
    (* Just linearly search for the key. We can't search more intelligently because we don't know
       the priority associated with [key]. *)
    let rec find_key i key =
        match get_key_opt heap i with
        | Some(other) ->
                if key = other then i
                else find_key (succ i) key
        | None -> raise (Key_not_found key)
    in
    let key_pos = find_key 0 key in
    heap.a.(key_pos) <- Node(priority, key);

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


let log2 a =
    log10 a /. log10 2.
;;


let get_level i = floor (log2 (float_of_int (succ i)))
;;


let print heap =
    let rec loop i =
        if i = heap.size then ()
        else
            (*
            0 -> 0   log(1)
            (1, 2) -> 1   floor(log(i + 1))
            (3, 4, 5, 6) -> 2
            *)
            match heap.a.(i) with
            | Node(priority, value) ->
                    let level = get_level i in
                    Printf.printf "(%d, %d) " priority value;
                    if get_level (succ i) > level then
                        Printf.printf "\n";
                    loop (succ i)
            | Empty -> ()
    in
    Printf.printf "size=%d\n" heap.size;
    loop 0;
    Printf.printf "\n\n"
;;
