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


(** Nodes is an array of (label, value). In our case, [label] is an [(x, y)] tuple, and should be
 * increasing--so node [i] has a label of [(i % width, i / width)].
 *)
let dijkstra nodes width height =
    let num_nodes = (Array.length nodes) in
    let q = Binheap.make num_nodes in
    let distance = Array.make num_nodes (-1) in (* -1 = INFINITY *)
    let prev = Array.make num_nodes (-1) in (* -1 = UNDEFINED *)
    (* TODO Binheap.init ? *)
    Array.iteri (fun i -> fun _ ->
        Binheap.insert q distance.(i) i
    ) nodes;
    let seen = Array.make num_nodes false in

    (* Returns list of neighbor indices in nodes *)
    let get_neighbors i =
        let x = i mod width in
        let y = i / width in
        (* let find_neighbor = function
            | x, y -> array_find_pos (fun node ->
                    let label, value = node in
                    label = (x, y)) nodes
        in *)
        let xy_to_i = function
            | x, y -> x * width + y
        in
        let neighbors = [] in
        let neighbors = if x > 0 then (x - 1, y) :: neighbors else neighbors in
        let neighbors = if y > 0 then (x, y - 1) :: neighbors else neighbors in
        let neighbors = if x < width - 1 then (x + 1, y) :: neighbors else neighbors in
        let neighbors = if y < height - 1 then (x, y + 1) :: neighbors else neighbors in
        List.map xy_to_i neighbors
    in

    let rec loop distance prev =
        if Binheap.empty q then distance, prev
        else begin
            let _udist, u = Binheap.extract q in
            let neighbors = get_neighbors u in
            let unseen_neighbors = List.filter (Array.get seen) neighbors in
            List.iter (fun v ->
                let _vlabel, vweight = nodes.(v) in
                let alt = distance.(u) + vweight in
                if alt < distance.(v) then
                    distance.(v) <- alt;
                    prev.(v) <- u;
                    Binheap.decrease_key q v alt
                ) unseen_neighbors;
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
    let board = helper [] in
    let width, height = get_matrix_dims board in

    (* Turn the board into a ((x, y), value) array *)
    let rec flatten_board costs x y =
        if y = height then costs
        else if x = width then flatten_board costs 0 (succ y)
        else
            let costs' = ((x, y), board.(x).(y)) :: costs in
            flatten_board costs' (succ x) y
    in
    List.rev (flatten_board [] 0 0)
;;

let do_game flat_board =
    ()
;;


let process_file filename =
    let _inc = open_in filename in
    Printf.printf "filename=%s\n" filename;

    Printf.printf "\n";
;;


let () =
    process_file "15-test.input";
    (*
    process_file "15.input";
    *)
;;
