
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



