

type t = {
    x: int;
    y: int;
}
;;

(** Make a new point.
    *)
let make x y =
    {x; y}
;;

(** Add two points (element-wise).
   *)
let add p1 p2 =
    {x = p1.x + p2.x; y = p1.y + p2.y}
;;


let compare p1 p2 =
    if p1.x = p2.x && p1.y = p2.y then
        0
    else if p1.x = p2.x then compare p1.y p2.y
    else compare p1.x p2.x
;;


let greater_than p1 p2 =
    compare p1 p2 = 1
;;
let gt = greater_than;;

let less_than p1 p2 =
    compare p1 p2 = -1
;;
let lt = less_than;;

let equal p1 p2 =
    compare p1 p2 = 0
;;
let eq = equal;;


let get_direction p1 p2 =
    let dx = p2.x - p1.x in
    let clamp n = 
        if n > 0 then 1
        else if n < 0 then -1
        else 0
    in
    let xdir = clamp dx in
    let dy = p2.y - p1.y in
    let ydir = clamp dy in
    {x = xdir; y = ydir}
;;


(** Returns a list of the points in the line between [p1] and [p2] (inclusive).
    *)
let points_in_line p1 p2 =
    let dir = get_direction p1 p2 in
    let start = p1 in
    (* Advance stop one further for convenience *)
    let stop = (add p2 dir) in
    let rec loop p ls =
        if equal p stop then
            ls
        else
            loop (add p dir) (p :: ls)
    in
    loop start []
;;


let fmt p =
    Printf.sprintf "{x = %d; y = %d}" p.x p.y
;;


(** "Draw" all the points in a matrix.
 *)
let fill_matrix points =
    let xmax, ymax =
        List.fold_left (fun acc -> fun point ->
            let xmax, ymax = acc in
            let xmax' = if point.x > xmax then point.x else xmax in
            let ymax' = if point.y > ymax then point.y else ymax in
            (xmax', ymax')
        ) (0, 0) points
    in
    let m = Array.make_matrix (xmax + 1) (ymax + 1) 0 in
    let rec helper points =
        match points with
        | [] -> m
        | hd :: rst ->
                m.(hd.x).(hd.y) <- 1;
                helper rst
    in
    (* x <-> col, y <-> row *)
    Utils.matrix_transpose (helper points)
;;
