

type point = {
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
    else if p1.x > p2.x && p1.y > p2.y then
        1
    else
        -1
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
