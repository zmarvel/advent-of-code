(*
--- Part Two ---

Now that you know how to find low-risk paths in the cave, you can try to find your way out.

The entire cave is actually five times larger in both dimensions than you thought; the area you originally scanned is just one tile in a 5x5 tile area that forms the full map. Your original map tile repeats to the right and downward; each time the tile repeats to the right or downward, all of its risk levels are 1 higher than the tile immediately up or left of it. However, risk levels above 9 wrap back around to 1. So, if your original map had some position with a risk level of 8, then that same position on each of the 25 total tiles would be as follows:

8 9 1 2 3
9 1 2 3 4
1 2 3 4 5
2 3 4 5 6
3 4 5 6 7

Each single digit above corresponds to the example position with a value of 8 on the top-left tile. Because the full map is actually five times larger in both dimensions, that position appears a total of 25 times, once in each duplicated tile, with the values shown above.

Here is the full five-times-as-large version of the first example above, with the original map in the top left corner highlighted:

11637517422274862853338597396444961841755517295286
13813736722492484783351359589446246169155735727126
21365113283247622439435873354154698446526571955763
36949315694715142671582625378269373648937148475914
74634171118574528222968563933317967414442817852555
13191281372421239248353234135946434524615754563572
13599124212461123532357223464346833457545794456865
31254216394236532741534764385264587549637569865174
12931385212314249632342535174345364628545647573965
23119445813422155692453326671356443778246755488935
22748628533385973964449618417555172952866628316397
24924847833513595894462461691557357271266846838237
32476224394358733541546984465265719557637682166874
47151426715826253782693736489371484759148259586125
85745282229685639333179674144428178525553928963666
24212392483532341359464345246157545635726865674683
24611235323572234643468334575457944568656815567976
42365327415347643852645875496375698651748671976285
23142496323425351743453646285456475739656758684176
34221556924533266713564437782467554889357866599146
33859739644496184175551729528666283163977739427418
35135958944624616915573572712668468382377957949348
43587335415469844652657195576376821668748793277985
58262537826937364893714847591482595861259361697236
96856393331796741444281785255539289636664139174777
35323413594643452461575456357268656746837976785794
35722346434683345754579445686568155679767926678187
53476438526458754963756986517486719762859782187396
34253517434536462854564757396567586841767869795287
45332667135644377824675548893578665991468977611257
44961841755517295286662831639777394274188841538529
46246169155735727126684683823779579493488168151459
54698446526571955763768216687487932779859814388196
69373648937148475914825958612593616972361472718347
17967414442817852555392896366641391747775241285888
46434524615754563572686567468379767857948187896815
46833457545794456865681556797679266781878137789298
64587549637569865174867197628597821873961893298417
45364628545647573965675868417678697952878971816398
56443778246755488935786659914689776112579188722368
55172952866628316397773942741888415385299952649631
57357271266846838237795794934881681514599279262561
65719557637682166874879327798598143881961925499217
71484759148259586125936169723614727183472583829458
28178525553928963666413917477752412858886352396999
57545635726865674683797678579481878968159298917926
57944568656815567976792667818781377892989248891319
75698651748671976285978218739618932984172914319528
56475739656758684176786979528789718163989182927419
67554889357866599146897761125791887223681299833479

Equipped with the full map, you can now find a path from the top left corner to the bottom right corner with the lowest total risk:

11637517422274862853338597396444961841755517295286
13813736722492484783351359589446246169155735727126
21365113283247622439435873354154698446526571955763
36949315694715142671582625378269373648937148475914
74634171118574528222968563933317967414442817852555
13191281372421239248353234135946434524615754563572
13599124212461123532357223464346833457545794456865
31254216394236532741534764385264587549637569865174
12931385212314249632342535174345364628545647573965
23119445813422155692453326671356443778246755488935
22748628533385973964449618417555172952866628316397
24924847833513595894462461691557357271266846838237
32476224394358733541546984465265719557637682166874
47151426715826253782693736489371484759148259586125
85745282229685639333179674144428178525553928963666
24212392483532341359464345246157545635726865674683
24611235323572234643468334575457944568656815567976
42365327415347643852645875496375698651748671976285
23142496323425351743453646285456475739656758684176
34221556924533266713564437782467554889357866599146
33859739644496184175551729528666283163977739427418
35135958944624616915573572712668468382377957949348
43587335415469844652657195576376821668748793277985
58262537826937364893714847591482595861259361697236
96856393331796741444281785255539289636664139174777
35323413594643452461575456357268656746837976785794
35722346434683345754579445686568155679767926678187
53476438526458754963756986517486719762859782187396
34253517434536462854564757396567586841767869795287
45332667135644377824675548893578665991468977611257
44961841755517295286662831639777394274188841538529
46246169155735727126684683823779579493488168151459
54698446526571955763768216687487932779859814388196
69373648937148475914825958612593616972361472718347
17967414442817852555392896366641391747775241285888
46434524615754563572686567468379767857948187896815
46833457545794456865681556797679266781878137789298
64587549637569865174867197628597821873961893298417
45364628545647573965675868417678697952878971816398
56443778246755488935786659914689776112579188722368
55172952866628316397773942741888415385299952649631
57357271266846838237795794934881681514599279262561
65719557637682166874879327798598143881961925499217
71484759148259586125936169723614727183472583829458
28178525553928963666413917477752412858886352396999
57545635726865674683797678579481878968159298917926
57944568656815567976792667818781377892989248891319
75698651748671976285978218739618932984172914319528
56475739656758684176786979528789718163989182927419
67554889357866599146897761125791887223681299833479

The total risk of this path is 315 (the starting position is still never entered, so its risk is not counted).

Using the full map, what is the lowest total risk of any path from the top left to the bottom right?
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
        let neighbors = [] in
        let neighbors = if x > 0 then i - 1 :: neighbors else neighbors in
        let neighbors = if y > 0 then i - width :: neighbors else neighbors in
        let neighbors = if x < width - 1 then i + 1 :: neighbors else neighbors in
        let neighbors = if y < height - 1 then i + width :: neighbors else neighbors in
        neighbors
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
    let big_width = 5 * width in
    let big_height = 5 * height in

    let rec tile_board big_board x y =
        if y = 5 then big_board
        else if x = 5 then tile_board big_board 0 (succ y)
        else
            let dstx = width * x in
            let dsty = height * y in
            let distance = x + y in
            let board' = matrix_map board (fun x -> ((x + distance - 1) mod 9) + 1) in
            matrix_blit board' (0, 0) big_board (dstx, dsty) (width, height);
            tile_board big_board (succ x) y
    in

    let big_board = Array.make_matrix big_width big_height 0 in
    let big_board = tile_board big_board 0 0 in

    (*
    Printf.printf "board=%s\n" (format_matrix_of_int board);
    Printf.printf "big_board=%s\n" (format_matrix_of_int big_board);
    *)

    (* Turn the board into a ((x, y), value) array *)
    let rec flatten_board board costs x y =
        let width, height = get_matrix_dims board in
        if y = height then costs
        else if x = width then flatten_board board costs 0 (succ y)
        else
            let costs' = ((x, y), board.(x).(y)) :: costs in
            flatten_board board costs' (succ x) y
    in
    (big_width, big_height), (List.rev (flatten_board big_board [] 0 0))
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
    let num_nodes = Array.length nodes in
    Printf.printf "width=%d height=%d num_nodes=%d\n" width height num_nodes;
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
