(*
--- Part Two ---

After reviewing the available paths, you realize you might have time to visit a single small cave twice. Specifically, big caves can be visited any number of times, a single small cave can be visited at most twice, and the remaining small caves can be visited at most once. However, the caves named start and end can only be visited exactly once each: once you leave the start cave, you may not return to it, and once you reach the end cave, the path must end immediately.

Now, the 36 possible paths through the first example above are:

start,A,b,A,b,A,c,A,end
start,A,b,A,b,A,end
start,A,b,A,b,end
start,A,b,A,c,A,b,A,end
start,A,b,A,c,A,b,end
start,A,b,A,c,A,c,A,end
start,A,b,A,c,A,end
start,A,b,A,end
start,A,b,d,b,A,c,A,end
start,A,b,d,b,A,end
start,A,b,d,b,end
start,A,b,end
start,A,c,A,b,A,b,A,end
start,A,c,A,b,A,b,end
start,A,c,A,b,A,c,A,end
start,A,c,A,b,A,end
start,A,c,A,b,d,b,A,end
start,A,c,A,b,d,b,end
start,A,c,A,b,end
start,A,c,A,c,A,b,A,end
start,A,c,A,c,A,b,end
start,A,c,A,c,A,end
start,A,c,A,end
start,A,end
start,b,A,b,A,c,A,end
start,b,A,b,A,end
start,b,A,b,end
start,b,A,c,A,b,A,end
start,b,A,c,A,b,end
start,b,A,c,A,c,A,end
start,b,A,c,A,end
start,b,A,end
start,b,d,b,A,c,A,end
start,b,d,b,A,end
start,b,d,b,end
start,b,end

The slightly larger example above now has 103 paths through it, and the even larger example now has 3509 paths through it.

Given these new rules, how many paths through this cave system are there?
*)


open Utils;;


exception Invalid_list_length of int


(**
   Convert list of length 2 into a pair.
   *)
let pair_of_list2 (ls : 'a list) : ('a * 'a) =
    match ls with
    | fst :: snd :: [] -> (fst, snd)
    | _ -> raise (Invalid_list_length (List.length ls))
;;


(* Load the edges into a list of edges *)
let load_file inc =
    let rec helper lines =
        match input_line_opt inc with
        | Some(line) ->
                let line = line
                    |> String.trim
                    |> String.split_on_char '-'
                    |> pair_of_list2
                in
                helper (line :: lines)
        | None -> lines
    in
    Array.of_list (helper [])
;;


(** Turn the edge list into an array of unique nodes *)
let get_nodes edges : string array =
    Array.fold_left (fun acc -> fun edge ->
            let a, b = edge in
            a :: b :: acc
    ) [] edges
        |> List.sort_uniq compare
        |> Array.of_list
;;


(** Populate a reverse-loopup table for [nodes], where the value in the resulting hash table refers
    to the index of the key in [nodes].
   *)
let reverse_nodes (nodes : string array) : (string, int) Hashtbl.t=
    (* Maps string to index in the nodes array *)
    let reverse_map = Hashtbl.create 50 in
    Array.iteri (fun i -> fun node ->
            Hashtbl.replace reverse_map node i;
    ) nodes;
    reverse_map
;;


(** Convert edges to numerical representations. In other words, if [nodes = [| "start"; "end" |]]
    and [edges = [| ("start", "end") |]], the result will be [[| (0, 1) |]]
   *)
let convert_edges (edges : (string * string) array) reverse_nodes : (int * int) array =
    Array.map (fun edge ->
        let start, stop = edge in
        let istart = Hashtbl.find reverse_nodes start in
        let istop = Hashtbl.find reverse_nodes stop in
        (istart, istop)) edges
;;


type 'a matrix = 'a array array;;

(** Creates a matrix [m] where [m.(i).(j)] and [m.(j).(i)] are 1 if the edge [i, j] exists.
    [nodes.(i)] is the node/string corresponding with the row [m.(i)].
   *)
let make_connected_matrix (edges : (int * int) array) num_nodes : int matrix =
    let m = Array.make_matrix num_nodes num_nodes 0 in
    Array.iter (fun edge ->
        let ei, ej = edge in
        m.(ei).(ej) <- 1;
        m.(ej).(ei) <- 1;
    ) edges;
    m
;;


let is_big_node node =
    string_for_all is_uppercase_ascii node
;;


let is_small_node node =
    string_for_all is_lowercase_ascii node
;;


let format_path nodes path =
    String.concat "," (List.map (Array.get nodes) path)
;;


let format_paths nodes paths =
    format_list paths (format_path nodes)
;;


(** True if [ls] has two elements meeting [pred].
    *)
let list_contains_two pred ls =
    let rec helper ls n =
        match ls with
        | hd :: rst ->
                let n' = if pred hd then (succ n) else n in
                if n' = 2 then true
                else helper rst n'
        | [] -> false
    in
    helper ls 0
;;


let list_find_duplicate_opt ls =
    let ls = List.sort compare ls in
    let rec helper ls =
        match ls with
        | fst :: snd :: rst -> if fst = snd then Some(fst) else helper (snd :: rst)
        | _ -> None
    in
    helper ls
;;


(** Find all valid paths from [start] to [stop]
   *)
let find_paths connected (nodes : string array) start stop : int list list =
    let num_nodes = Array.length nodes in
    (* Returns a list of paths from curr to stop. *)
    let rec helper paths path curr _prev =
        if curr = stop then ((curr :: path) :: paths)
        else
            (* For each connected node, call the function recursively. Connected nodes are
               disqualified if they are small caves that have already been seen (this prevents us
               from looping forever in small caves), or if they are big caves that were just seen
               (prev).  *)
            let curr_connected = connected.(curr) in
            let rec loop (j : int) paths : int list list =
                if j = num_nodes then paths
                else
                    let new_paths =
                        if j = start then []
                        else if is_small_node nodes.(j) && (
                            let small_in_path = List.filter (fun i -> is_small_node nodes.(i)) path in
                            let duplicate_opt = list_find_duplicate_opt small_in_path in
                            match duplicate_opt with
                            | Some (duplicate) -> duplicate = j (* If duplicate = j, then adding this small cave again would lead to us visiting it twice. *)
                            | None -> false
                                (* match duplicate_opt with
                                | Some(duplicate) -> duplicate = j
                                | None -> false *)) then []
                        else if curr_connected.(j) = 0 then []
                        else helper [] (curr :: path) j curr
                    in
                    (* Printf.printf "paths=%s\nnew_paths=%s\n\n" (format_paths nodes paths)
                       (format_paths nodes new_paths); *)
                    loop (succ j) (new_paths @ paths)
            in
            (loop 0 []) @ paths
    in
    helper [] [] start (-1)
;;


let do_game edges =
    let nodes = get_nodes edges in
    let node_index = reverse_nodes nodes in
    let edge_ints = convert_edges edges node_index in
    let num_nodes = Array.length nodes in
    let connected = make_connected_matrix edge_ints num_nodes in
    Printf.printf "nodes=%s\n" (format_array_of_string nodes);
    Printf.printf "connected=%s\n" (format_matrix_of_int connected);
    let start_pos = array_find_pos (fun x -> x = "start") nodes in
    let stop_pos = array_find_pos (fun x -> x = "end") nodes in
    let paths = find_paths connected nodes start_pos stop_pos in
    let paths = List.rev (List.map List.rev paths) in
    let sorted_paths = List.sort (fun l1 -> fun l2 ->
        let len_l1 = List.length l1 in
        let len_l2 = List.length l2 in
        if len_l1 = len_l2 then
            List.fold_left2 (fun acc -> fun a -> fun b ->
                if acc = 0 then compare a b
                else acc
            ) 0 l1 l2
        else compare len_l1 len_l2
        ) paths in
    Printf.printf "paths=[\n";
    List.iter (fun path -> Printf.printf "%s\n" (format_path nodes path)) paths;
    Printf.printf "]\n";
    Printf.printf "paths=[\n";
    List.iter (fun path -> Printf.printf "%s\n" (format_path nodes path)) sorted_paths;
    Printf.printf "]\n";

    List.length paths
;;



let process_file filename =
    let inc = open_in filename in
    Printf.printf "filename=%s\n" filename;
    let edges = load_file inc in
    let result = do_game edges in
    Printf.printf "result=%d\n" result;

    Printf.printf "\n";
;;

let () =
    process_file "12-test1.input";
    (* process_file "12-test2.input"; *)
    (* process_file "12-test3.input"; *)
    (* process_file "12.input"; *)
;;
