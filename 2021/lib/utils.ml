
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


exception Invalid_digit of char


(** [ctoi '1'] will return [1].
 *)
let ctoi c =
    let lower = int_of_char '0' in
    let upper = int_of_char '9' in
    let x = int_of_char c in
    if x >= lower && x <= upper then x - lower
    else raise (Invalid_digit c)
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


(** Convert an integer to a string representing a binary number.
 * [int_to_binary_string 10 6] returns ["001010"].
 *)
let int_to_binary_string n num_bits =
        String.init num_bits (function i ->
                char_of_int (((n lsr (num_bits - i - 1)) land 1) + int_of_char '0')
        )
;;


(** Integer division, with the result rounded up *)
let divceil x y =
    (x + (y - 1)) / y
;;


let ctoi_base base c =
    let x =
        let base_offset, base_char =
            if c >= '0' && c <= '9' then (0, '0')
            else if c >= 'a' && c <= 'z' then (10, 'a')
            else if c >= 'A' && c <= 'Z' then (10, 'A')
            else raise (Invalid_digit c)
        in
        base_offset + ((int_of_char c) - (int_of_char base_char))
    in
    if x < base then x
    (* else raise (Invalid_digit (char_of_int x)) *)
    else raise (Invalid_digit c)
;;


let hex_string_to_bytes s =
    String.init (String.length s / 2) (fun i ->
        let hi = ctoi_base 16 s.[i] in
        let lo = ctoi_base 16 s.[i+1] in
        char_of_int ((hi lsl 4) lor lo)
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


let array_ifor_all (pred : int -> 'a -> bool) (a : 'a array) : bool =
    let n = Array.length a in
    let rec loop i =
        if i = n then true
        else (pred i a.(i)) && loop (succ i)
    in
    loop 0
;;


(** Element-wise vector addition.
 *)
let array_add a1 a2 =
    Array.map2 (+) a1 a2
;;


let array_sub a1 a2 =
    Array.map2 (-) a1 a2
;;


(** Element-wise vector multiplication.
 *)
let array_multiply a1 a2 =
    Array.map2 ( * ) a1 a2
;;


(** Slide all elements in a to the by left [num_slots] slots (creates a new array). Elements that
 * don't get replaced are filled with [fill].
    *)
let array_slide_left num_slots fill a =
    let n = Array.length a in
    Array.mapi (fun i -> fun _ ->
        if i < n - num_slots then
            a.(i + num_slots)
        else
            fill
    ) a
;;

(** Slide all elements in a to the by right [num_slots] slots (creates a new array). Elements that
 * don't get replaced are filled with [fill].
    *)
let array_slide_right num_slots fill a =
    Array.mapi (fun i -> fun _ ->
        if i >= num_slots then
            a.(i - num_slots)
        else
            fill
    ) a
;;


let array_abs a =
    Array.map abs a
;;


(** Add a scalar [x] to all elements of an array.
   *)
let array_add_scalar a x =
    Array.map (fun y -> x + y) a
;;


(** Subtract a scalar [x] from all elements of an array.
   *)
let array_sub_scalar a x =
    array_add_scalar a ((-1) * x)
;;


(** Calculate the sum of all elements of an array of int.
   *)
let array_sum a =
    Array.fold_left (+) 0 a
;;


(** Return an array counting from [start] (inclusive) to [stop] (exclusive).
   *)
let array_iota start stop =
    if start < stop then
        let n = stop - start in
        Array.init n (fun i -> start + i)
    else raise (Invalid_argument "array_iota")
;;


let array_reverse a =
    let len = Array.length a in
    Array.init len (function i -> a.(len - i - 1))
;;


(* Produces an array representing a mask [m], where [m[i]] is 1 when [a[i] > threshold].
 *)
let array_greater_than threshold a =
    Array.map (fun x -> if x > threshold then 1 else 0) a
;;


let array_less_than threshold a =
    Array.map (fun x -> if x < threshold then 1 else 0) a
;;


let array_equal value a =
    Array.map (fun x -> if x = value then 1 else 0) a
;;


(* Produces an array representing a mask [m], where [m[i]] is 1 when [a1[i] > a2[i]].
 *)
let array_greater_than2 a1 a2 =
    Array.map2 (fun x -> fun y -> if x > y then 1 else 0) a1 a2
;;


let array_less_than2 a1 a2 =
    Array.map2 (fun x -> fun y -> if x < y then 1 else 0) a1 a2
;;


let array_equal2 a1 a2 =
    Array.map2 (fun x -> fun y -> if x = y then 1 else 0) a1 a2
;;


(** Produces array [a'], where [a'.(i) = if mask(i) > 0 then a.(i) else 0].
 *)
let array_mask a mask =
    Array.map2 (fun x -> fun m ->
        if m > 0 then x
        else 0
    ) a mask
;;


(* Values greater than 0 are set to 1 and everything else is set to 0.
   *)
let array_to_binary =
    array_greater_than 0
;;


(** Split an array of pairs into a pair of arrays. Introduced in OCaml 4.13.0.
    *)
let array_split (a : ('a * 'b) array) : ('a array * 'b array)  =
    let a1 =
        Array.init (Array.length a) (fun i ->
            let x1, _ = a.(i) in
            x1)
    in
    let a2 =
        Array.init (Array.length a) (fun i ->
            let _, x2 = a.(i) in
            x2)
    in
    (a1, a2)
;;


(** Introduced in OCaml 4.13.0 *)
let array_find_opt (pred : 'a -> bool) (a : 'a array) : 'a option =
    let n = Array.length a in
    let rec loop i =
        if i = n then
            None
        else
            let x = a.(i) in
            if pred x then Some(x)
            else loop (succ i)
    in
    loop 0
;;


let array_find pred a =
    Option.get (array_find_opt pred a)
;;


let array_find_pos_opt pred a =
    let n = Array.length a in
    let rec loop i =
        if i = n then None
        else if pred a.(i) then Some(i)
        else loop (succ i)
    in
    loop 0
;;


let array_find_pos pred a =
    Option.get (array_find_pos_opt pred a)
;;


(** Convert string to char array *)
let array_of_string s =
        Array.init (String.length s) (String.get s)
;;


let init_matrix dim1 dim2 (f : int -> int -> int) =
    Array.init dim1 (function i -> Array.init dim2 (f i))
;;


let get_matrix_dims m =
    let rows = Array.length m in
    let cols = Array.length m.(0) in
    rows, cols
;;


let matrix_transpose m =
    let rows, cols = get_matrix_dims m in
    init_matrix cols rows (fun i -> fun j -> m.(j).(i))
;;


(* exception Invalid_dims of (int * int) *)
exception Invalid_dims2 of ((int * int) * (int * int))


(**
 * Returns a new matrix resulting from applying [f] to each element of [m1] and [m2].
 *
 * TODO: Consider changing argument order for consistency with Array.map2 (function arg first)
 *)
let matrix_map2 m1 m2 f =
    let dims1 = get_matrix_dims m1 in
    let dims2 = get_matrix_dims m2 in
    if dims1 = dims2 then
        let dim1, dim2 = get_matrix_dims m1 in
        init_matrix dim1 dim2 (fun i -> fun j ->
            (* Printf.printf "i=%d j=%d row1=%d row2=%d\n" i j (Array.length m1.(i)) (Array.length m2.(i)) ; *)
            f m1.(i).(j) m2.(i).(j))
    else raise (Invalid_dims2 (dims1, dims2))
;;

(** Element-wise matrix multiplication *)
let matrix_multiply m1 m2 =
    matrix_map2 m1 m2 ( * )
;;

(** Element-wise matrix addition *)
let matrix_add m1 m2 =
    matrix_map2 m1 m2 (+)
;;

(** Element-wise matrix subtraction *)
let matrix_sub m1 m2 =
    matrix_map2 m1 m2 (-)
;;


(** Apply [f] to all cells of [m], returning the result in a new matrix. *)
let matrix_map m f =
    Array.map (fun row -> Array.map f row) m
;;


let matrix_fold_left f init m =
    Array.fold_left (fun acc -> fun row ->
        Array.fold_left f acc row) init m
;;


let matrix_where pred m =
    let num_rows = Array.length m in
    let num_cols = Array.length m.(0) in
    let rec helper i j ls =
        if i = num_rows then ls
        else if j = num_cols then helper (succ i) 0 ls
        else
            let ls' = if pred m.(i).(j) then ((i, j) ::  ls) else ls in
            helper i (succ j) ls'
    in
    helper 0 0 []
;;


(* TODO: rename matrix_add to matrix_add2, I guess *)
let matrix_add_scalar m x =
    matrix_map m (fun y -> x + y)
;;


let matrix_sum m =
    Array.fold_left (fun acc -> fun row ->
        acc + (array_sum row)) 0 m
;;


(** Produces a mask of values in [m] equal to [value], where [m.(i) = 1] iff [m.(i) > value]
 * (otherwise 0).
 *)
let matrix_greater_than m threshold =
    Array.map (array_greater_than threshold) m
;;


(** Produces a mask of values in [m] equal to [value], where [m.(i) = 1] iff [m.(i) < value]
 * (otherwise 0).
 *)
let matrix_less_than m threshold =
    Array.map (array_less_than threshold) m
;;


(** Produces a mask of values in [m] equal to [value], where [m.(i) = 1] iff [m.(i) = value]
 * (otherwise 0).
 *)
let matrix_equal m value =
    Array.map (array_equal value) m
;;


(** Produces a matrix where cell [i] is 1 if [m1.(i) > m2.(i)], and otherwise 0.
 *)
let matrix_greater_than2 m1 m2 =
    Array.map2 array_greater_than2 m1 m2
;;

(** Produces a matrix where cell [i] is 1 if [m1.(i) < m2.(i)], and otherwise 0.
 *)
let matrix_less_than2 m1 m2 =
    Array.map2 array_less_than2 m1 m2
;;

(** Produces a matrix where cell [i] is 1 if [m1.(i) = m2.(i)], and otherwise 0.
 *)
let matrix_equal2 m1 m2 =
    Array.map2 array_equal2 m1 m2
;;


let matrix_mask m mask =
    matrix_map2 m mask (fun x -> fun m ->
        if m > 0 then x
        else 0
    )
;;


let matrix_not m =
    matrix_equal m 0
;;


(** "Logical OR" of two int matrices representing a mask. In other words, the output [m.(i).(j) = 1]
    iff [m1.(i).(j) = 1 || m2.(i).(j) = 1], otherwise 0.
   *)
let matrix_lor2 m1 m2 =
    matrix_map2 m1 m2 (fun x -> fun y -> if x = 1 || y = 1 then 1 else 0)
;;


(** Logical xor.
   *)
let xor (x : bool) (y : bool) : bool =
    (x && not y) || (not x && y)
;;


(** "Logical XOR" of two int matrices representing a mask. In other words, the output [m.(i).(j) = 1]
    iff [m1.(i).(j) = 1 lxor m2.(i).(j) = 1], otherwise 0.
   *)
let matrix_lxor2 m1 m2 =
    matrix_map2 m1 m2 (fun x -> fun y -> if xor (x = 1) (y = 1) then 1 else 0)
;;


(** For every row in [m], move the elements left by [num_cols]. For elements on the right end of
 * the row that don't get replaced, fill them with [fill].
 *)
let matrix_slide_left num_cols fill m =
    Array.map (array_slide_left num_cols fill) m
;;


(** For every row in [m], move the elements left by [num_cols]. For elements on the right end of
 * the row that don't get replaced, fill them with [fill].
 *)
let matrix_slide_right num_cols fill m =
    Array.map (array_slide_right num_cols fill) m
;;


let matrix_slide_up num_rows fill m =
    let n = Array.length m in
    let ncols = Array.length m.(0) in
    Array.mapi (fun i -> fun _ ->
        if i < n - num_rows then
            Array.copy m.(i + num_rows)
        else
            Array.make ncols fill
    ) m
;;


let matrix_slide_down num_rows fill m =
    let ncols = Array.length m.(0) in
    Array.mapi (fun i -> fun _ ->
        if i >= num_rows then
            Array.copy m.(i - num_rows)
        else
            Array.make ncols fill
    ) m
;;


let matrix_blit msrc src_pos mdst dst_pos len =
    let srci, srcj = src_pos in
    let dsti, dstj = dst_pos in
    let rows, cols = len in
    let rec loop i =
        if i = rows then ()
        else begin
            Array.blit msrc.(srci + i) srcj mdst.(dsti + i) dstj cols;
            loop (succ i)
        end
    in
    loop 0
;;


let is_lowercase_ascii (c : char) =
    c >= 'a' && c <= 'z'
;;


let is_uppercase_ascii (c : char ) =
    c >= 'A' && c <= 'Z'
;;


let string_reverse s =
    let len = String.length s in
    String.init len (function i -> s.[len - i - 1])
;;


let string_fold_left (f : 'a -> char -> 'a) (init : 'a) (s : string) : 'a =
    let n = String.length s in
    let rec loop i acc =
        if i = n then acc
        else loop (succ i) (f acc s.[i])
    in
    loop 0 init
;;


(** Introduced in OCaml 4.13.0 *)
let string_for_all (pred : char -> bool) (s : string): bool =
    let n = String.length s in
    let rec helper i =
        if i = n then
            true
        else
            (pred s.[i]) && (helper (succ i))
    in
    helper 0
;;


let format_pair_of_int (p : int * int) =
    let x, y = p in
    Printf.sprintf "(%d, %d)" x y
;;


let format_list ls fmt =
    let b = Buffer.create 16 in
    Buffer.add_string b "[ ";
    List.iter (fun n -> Buffer.add_string b (fmt n); Buffer.add_string b " ";) ls;
    Buffer.add_string b "]";
    Buffer.contents b
;;


(** Format a list of integers as a string. Does not include a final newline. *)
let format_list_of_int ls =
    format_list ls (Printf.sprintf "%d")
;;


let format_list_of_string ls =
    format_list ls (fun s -> s)
;;


let format_array a fmt =
    let b = Buffer.create 16 in
    Buffer.add_string b "[| ";
    Array.iter (fun n -> Buffer.add_string b (fmt n); Buffer.add_string b " ";) a;
    Buffer.add_string b "|]";
    Buffer.contents b
;;


let format_array_of_int a =
    format_array a (Printf.sprintf "%d")
;;


let format_array_of_string a =
    format_array a (fun x -> x)
;;


let format_array_of_char a =
        format_array a (String.make 1)
;;


let format_matrix_of_int m =
    let b = Buffer.create 16 in
    Buffer.add_string b "[| \n";
    Array.iter (fun row ->
        Buffer.add_string b "   ";
        Buffer.add_string b (format_array_of_int row);
        Buffer.add_string b "\n") m;
    Buffer.add_string b "|]";
    Buffer.contents b
;;


let rec list_contains pred ls =
    match ls with
    | hd :: rst -> pred hd || list_contains pred rst
    | _ -> false
;;


let string_of_char = String.make 1;;


(** Turn a list of chars into a string *)
let join_chars ls =
    let strings = List.rev_map string_of_char ls in
    String.concat "" (List.rev strings)
;;


(** Turn a string into a list of chars *)
let explode_chars s =
    let n = String.length s in
    let rec loop i chars =
        if i < 0 then chars
        else loop (pred i) (s.[i] :: chars)
    in
    loop (n - 1) []
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
