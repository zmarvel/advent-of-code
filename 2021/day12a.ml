(*
--- Day 12: Passage Pathing ---

With your submarine's subterranean subsystems subsisting suboptimally, the only way you're getting out of this cave anytime soon is by finding a path yourself. Not just a path - the only way to know if you've found the best path is to find all of them.

Fortunately, the sensors are still mostly working, and so you build a rough map of the remaining caves (your puzzle input). For example:

start-A
start-b
A-c
A-b
b-d
A-end
b-end

This is a list of how all of the caves are connected. You start in the cave named start, and your destination is the cave named end. An entry like b-d means that cave b is connected to cave d - that is, you can move between them.

So, the above cave system looks roughly like this:

    start
    /   \
c--A-----b--d
    \   /
     end

Your goal is to find the number of distinct paths that start at start, end at end, and don't visit small caves more than once. There are two types of caves: big caves (written in uppercase, like A) and small caves (written in lowercase, like b). It would be a waste of time to visit any small cave more than once, but big caves are large enough that it might be worth visiting them multiple times. So, all paths you find should visit small caves at most once, and can visit big caves any number of times.

Given these rules, there are 10 paths through this example cave system:

start,A,b,A,c,A,end
start,A,b,A,end
start,A,b,end
start,A,c,A,b,A,end
start,A,c,A,b,end
start,A,c,A,end
start,A,end
start,b,A,c,A,end
start,b,A,end
start,b,end

(Each line in the above list corresponds to a single path; the caves visited by that path are listed in the order they are visited and separated by commas.)

Note that in this cave system, cave d is never visited by any path: to do so, cave b would need to be visited twice (once on the way to cave d and a second time when returning from cave d), and since cave b is small, this is not allowed.

Here is a slightly larger example:

dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc

The 19 paths through it are as follows:

start,HN,dc,HN,end
start,HN,dc,HN,kj,HN,end
start,HN,dc,end
start,HN,dc,kj,HN,end
start,HN,end
start,HN,kj,HN,dc,HN,end
start,HN,kj,HN,dc,end
start,HN,kj,HN,end
start,HN,kj,dc,HN,end
start,HN,kj,dc,end
start,dc,HN,end
start,dc,HN,kj,HN,end
start,dc,end
start,dc,kj,HN,end
start,kj,HN,dc,HN,end
start,kj,HN,dc,end
start,kj,HN,end
start,kj,dc,HN,end
start,kj,dc,end

Finally, this even larger example has 226 paths through it:

fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW

How many paths through this cave system are there that visit small caves at most once?
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
    format_list path (fun i -> nodes.(i))
;;

let format_paths nodes paths =
    format_list paths (format_path nodes)
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
                        if is_small_node nodes.(j) && Option.is_some (List.find_opt (fun x -> x = j) path) then []
                        else if curr_connected.(j) = 1 then helper [] (curr :: path) j curr
                        else []
                    in
                    (* Printf.printf "paths=%s\nnew_paths=%s\n\n" (format_paths nodes paths)
                       (format_paths nodes new_paths); *)
                    loop (succ j) (new_paths @ paths)
            in
            (loop 0 []) @ paths
    in
    helper [] [] start (-1)
;;


(*
helper [] start -1
    helper [start] A start -> [[c; A; start]; [end; A; start]; [d; b; A; start]; [end; b; A; start]]
        helper [A; start] c A -> [[c; A; start]] (X)
        helper [A; start] end A -> [[end; A; start]]
        helper [A; start] b A -> [[d; b; A; start]; [end; b; A; start]]
            helper [b; A; start] d b -> [[d; b; A; start]] (X)
            helper [b; A; start] end b -> [[end; b; A; start]]
    helper [start] b start -> [[c; A; b; start]; [end; A; b; start]; [end; b; start]]
        helper [b; start] A b -> [[c; A; b; start]; [end; A; b; start]]
            helper [A; b; start] c A -> [[c; A; b; start]] (X)
            helper [A; b; start] end A -> [[end; A; b; start]]
        helper [b; start] end b -> [[end; b; start]]
*)


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
    Printf.printf "paths=%s\n" (format_paths nodes paths);
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
    process_file "12-test2.input";
    process_file "12-test3.input";
    process_file "12.input";
;;
