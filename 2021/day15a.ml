(*
--- Day 15: Chiton ---
You've almost reached the exit of the cave, but the walls are getting closer together. Your submarine can barely still fit, though; the main problem is that the walls of the cave are covered in chitons, and it would be best not to bump any of them.

The cavern is large, but has a very low ceiling, restricting your motion to two dimensions. The shape of the cavern resembles a square; a quick scan of chiton density produces a map of risk level throughout the cave (your puzzle input). For example:

1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581
You start in the top left position, your destination is the bottom right position, and you cannot move diagonally. The number at each position is its risk level; to determine the total risk of an entire path, add up the risk levels of each position you enter (that is, don't count the risk level of your starting position unless you enter it; leaving it adds no risk to your total).

Your goal is to find a path with the lowest total risk. In this example, a path with the lowest total risk is highlighted here:

1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581
The total risk of this path is 40 (the starting position is never entered, so its risk is not counted).

What is the lowest total risk of any path from the top left to the bottom right?
*)


open Utils;;


exception Invalid_input_file of string


let i_to_xy width i =
    let x = i mod width in
    let y = i / width in
    x, y
;;
let xy_to_i width = function
    | x, y -> y * width + x
;;



(** Nodes is an array of (label, value). In our case, [label] is an [(x, y)] tuple, and should be
 * increasing--so node [i] has a label of [(i % width, i / width)].
 *)
let dijkstra nodes width height source _target =
    let num_nodes = (Array.length nodes) in
    let _node_label, node_weight = array_split nodes in
    let q = Binheap.make num_nodes in
    let distance = Array.make num_nodes 999999 in (* 999999 = INFINITY *)
    let prev = Array.make num_nodes (-1) in (* -1 = UNDEFINED *)
    (* TODO Binheap.init ? *)
    Array.iteri (fun i -> fun _ ->
        Binheap.insert q distance.(i) i
    ) nodes;
    let seen = Array.make num_nodes false in

    distance.(source) <- 0;
    seen.(source) <- true;
    Binheap.decrease_key q node_weight.(source) 0;

    (* Returns list of neighbor indices in nodes *)
    let get_neighbors i =
        let x, y = i_to_xy width i in
        (* let find_neighbor = function
            | x, y -> array_find_pos (fun node ->
                    let label, value = node in
                    label = (x, y)) nodes
        in *)
        (* TODO: Don't really need the conversions--can just add and subtract 1 and width *)
        let neighbors = [] in
        let neighbors = if x > 0 then (x - 1, y) :: neighbors else neighbors in
        let neighbors = if y > 0 then (x, y - 1) :: neighbors else neighbors in
        let neighbors = if x < width - 1 then (x + 1, y) :: neighbors else neighbors in
        let neighbors = if y < height - 1 then (x, y + 1) :: neighbors else neighbors in
        List.map (xy_to_i width) neighbors
    in

    let _print_as_grid a width height =
        (* Printf.printf "%s\n" (format_array_of_int a); *)
        let rec loop i j =
            if i = height then ()
            else if j = width then (Printf.printf "\n"; loop (succ i) 0)
            else (Printf.printf "% 7d " a.(i * width + j); loop i (succ j))
        in
        loop 0 0
    in

    let rec loop distance prev =
        if Binheap.empty q then distance, prev
        else begin
            let _udist, u = Binheap.extract q in
            let neighbors = get_neighbors u in
            let unseen_neighbors = List.filter (fun i -> not seen.(i)) neighbors in
            (*
            Printf.printf "----------------------------\n";
            Printf.printf "neighbors=%s\n" (format_list_of_int neighbors);
            Printf.printf "unseen_neighbors=%s\n" (format_list_of_int unseen_neighbors);
            *)
            let visit_neighbor v =
                let vweight = node_weight.(v) in
                let alt = distance.(u) + vweight in
                (* Printf.printf "u=%d/%s[%d] v=%d/%s[%d] alt=%d\n" u (format_pair_of_int (i_to_xy
                   width u)) distance.(u) v (format_pair_of_int (i_to_xy width v)) distance.(v) alt;
                   *)
                if alt < distance.(v) then begin
                    (* Printf.printf "decrease_key %d %d->%d\n" v distance.(v) alt; *)
                    distance.(v) <- alt;
                    prev.(v) <- u;
                    Binheap.decrease_key q alt v;
                    (* Binheap.print q *)
                end
            in
            List.iter visit_neighbor unseen_neighbors;
            seen.(u) <- true;
            (*
            Printf.printf "distance=\n";
            print_as_grid distance width height;
            Printf.printf "prev=\n";
            print_as_grid prev width height;
            Printf.printf "node_weight=\n";
            print_as_grid node_weight width height;
            *)
            loop distance prev
        end
    in
    loop distance prev
;;


let load_file inc =
    let rec helper rows =
        match input_line_opt inc with
        | Some(line) ->
                let line = String.trim line in
                let costs = Array.map ctoi (array_of_string line) in
                helper (costs :: rows)
        | None -> array_reverse (Array.of_list rows)
    in
    let board = matrix_transpose (helper []) in
    let width, height = get_matrix_dims board in

    (* Turn the board into a ((x, y), value) array *)
    let rec flatten_board costs x y =
        if y = height then costs
        else if x = width then flatten_board costs 0 (succ y)
        else
            let costs' = ((x, y), board.(x).(y)) :: costs in
            flatten_board costs' (succ x) y
    in
    (width, height), (List.rev (flatten_board [] 0 0))
;;

let do_game flat_board width height =
    let source = 0 in
    let target = (Array.length flat_board) - 1 in
    let distance, _prev = dijkstra flat_board width height source target in
    distance.(target)
;;


let process_file filename =
    let inc = open_in filename in
    Printf.printf "filename=%s\n" filename;
    let dims, node_list = load_file inc in
    let nodes = Array.of_list node_list in
    let width, height = dims in
    let result = do_game nodes width height in
    Printf.printf "result=%d\n" result;

    Printf.printf "\n";
;;


let () =
    process_file "15-test1.input";
    process_file "15-test2.input";
    process_file "15.input";
    (*
    *)
;;
