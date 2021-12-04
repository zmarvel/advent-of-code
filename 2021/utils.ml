
(** Given an in channel, return either [Some(line)] (newline included) or [None].
 *)
let input_line_opt inc =
    try
        let line = input_line inc in
        Some(line)
    with
    | End_of_file -> None
;;


(* Integer pow function, from https://stackoverflow.com/questions/16950687/integer-exponentiation-in-ocaml *)
let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n ->
    let b = pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a)
;;


(** [ctoi '1'] will return [1].
 *)
let ctoi c =
    (int_of_char c) - (int_of_char '0')
;;


(** [binary_string_to_int "1000"] returns 8.
 *)
let binary_string_to_int s =
    let rec loop i n =
        if i == String.length s then
            n
        else
            loop (i + 1) (n + (ctoi (String.get s i)) * (pow 2 ((String.length s) - i - 1)))
    in
    loop 0 0
;;


(** Print an array, with no trailing newline.
 *
 *  TODO: Just return a string
 *)
let print_array a =
    Printf.printf "[ ";
    Array.iter (Printf.printf "%d ") a;
    Printf.printf "]";
;;


(** Convert an integer to a string representing a binary number.
 * [int_to_binary_string 10 6] returns ["001010"].
 *)
let int_to_binary_string n num_bits =
        String.init num_bits (function i ->
                char_of_int (((n lsr (num_bits - i - 1)) land 1) + int_of_char '0')
        )
;;


(** Convert an integer to an array, where each element is one bit of the original integer. For
 * example, [explode_int 10 6] returns [[| 0; 0; 1; 0; 1; 0 |]].
 *)
let explode_int n num_bits =
    let parts = Array.make num_bits 0 in
    let rec loop i =
        if i = num_bits then
            ()
        else begin
            Array.set parts i ((n lsr i) land 1);
            loop (i + 1)
        end
    in
    loop 0;
    parts
;;

(* Values equal to 0 are set to 1 and everything else is set to 0.
   *)
let invert_array a =
    Array.map (fun x -> if x == 0 then 1 else 0) a
;;


(** Element-wise vector addition.
 *)
let add_array a1 a2 =
    (* Just assume they're the same length *)
    let out = Array.make (Array.length a1) 0 in
    Array.iteri (fun i x -> Array.set out i ((Array.get a2 i) + x)) a1;
    out
;;



(* Produces an array representing a mask [m], where [m[i]] is 1 when [a[i] > threshold].
 *)
let array_greater_than threshold a =
    Array.map (fun x -> if x > threshold then 1 else 0) a
;;


(* Values greater than 0 are set to 1 and everything else is set to 0.
   *)
let array_to_binary =
    array_greater_than 0
;;


(**
 * Assuming [a] is a sorted array, find the first element [x] between indices [start] and [stop]
 * where [pred x] is true, and return its index.
 *
 * TODO: This is a linear scan, but it could be a binary search.
 *)
let partition_point a start stop pred =
    let rec loop i =
        if i = stop then
            i
        else
            let x = a.(i) in
            if pred x then
                i
            else
                loop (succ i)
    in
    loop start
;;
