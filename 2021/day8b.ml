(*
--- Part Two ---
Through a little deduction, you should now be able to determine the remaining digits. Consider again the first example above:

acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab |
cdfeb fcadb cdfeb cdbaf
After some careful analysis, the mapping between signal wires and segments only make sense in the following configuration:

 dddd
e    a
e    a
 ffff
g    b
g    b
 cccc
So, the unique signal patterns would correspond to the following digits:

acedgfb: 8
cdfbe: 5
gcdfa: 2
fbcad: 3
dab: 7
cefabd: 9
cdfgeb: 6
eafb: 4
cagedb: 0
ab: 1
Then, the four digits of the output value can be decoded:

cdfeb: 5
fcadb: 3
cdfeb: 5
cdbaf: 3
Therefore, the output value for this entry is 5353.

Following this same process for each entry in the second, larger example above, the output value of each entry can be determined:

fdgacbe cefdb cefbgd gcbe: 8394
fcgedb cgb dgebacf gc: 9781
cg cg fdcagb cbg: 1197
efabcd cedba gadfec cb: 9361
gecf egdcabf bgf bfgea: 4873
gebdcfa ecba ca fadegcb: 8418
cefg dcbef fcge gbcadfe: 4548
ed bcgafe cdgba cbgef: 1625
gbdfcae bgc cg cgb: 8717
fgae cfgab fg bagce: 4315
Adding all of the output values in this larger example produces 61229.

For each entry, determine all of the wire/segment connections and decode the four-digit output values. What do you get if you add up all of the output values?
*)


open Utils;;


(** Load an array of fish positions
 *)
let load_file inc =
    let rec helper entries =
        match input_line_opt inc with
        | Some(line) ->
                let input_output =
                    line
                    |> String.split_on_char '|'
                    |> List.map String.trim
                    (* Make a list of lists, where the first list is the inputs and the second list is the outputs *)
                    |> List.map (String.split_on_char ' ')
                    |> List.map Array.of_list
                in
                let input = List.hd input_output in
                let output = List.nth input_output 1 in
                helper ((input, output) :: entries)
        | None -> entries
    in
    array_reverse (Array.of_list (helper []))
;;


(** Assumes s1 and s2 are the same length. Produces a list of pairs [(s1.[i], s2.[i])] *)
let zip_strings s1 s2 =
    if String.length s1 <> String.length s2 then
        Printf.printf "ERROR: Unmatched strings: %s %s\n" s1 s2;
    let rec loop i =
        if i < 0 then
            []
        else
            (s1.[i], s2.[i]) :: (loop (pred i))
    in
    loop (String.length s1 - 1)
;;


let find_unique_segments (displays : string array) =
    Array.fold_left (fun acc -> fun num ->
        let new_pairs =
            match String.length num with
            (* We need the association in reverse: decode instead of encode *)
            | 2 (* 1 *) -> zip_strings num "cf"
            | 4 (* 4 *) -> zip_strings num "bcdf"
            | 3 (* 7 *) -> zip_strings num "acf"
            | 7 (* 8 *) -> zip_strings num "abcdefg"
            | _ -> []
        in
        (* TODO: How does the order affect complexity here? *)
        List.rev_append new_pairs acc
    ) [] displays
;;

let count_unique_segments (displays : string array) =
    Array.fold_left (fun acc -> fun num ->
        let new_pairs =
            match String.length num with
            | 2 | 4 | 3 | 7 (* 1, 4, 7, 8 *) -> 1
            | _ -> 0
        in
        acc + new_pairs
    ) 0 displays
;;

let convert_char c =
    (int_of_char c) - (int_of_char 'a')
;;


(*
 * s = "be"
 *
 * c1 = convert_char 'b' = 1
 * c2 = convert_char 'e' = 4
 *
 * output = 0b0100100
 *)
let encode_string s =
    let n = String.length s in
    let num_bits = 7 in
    let rec loop i encoded =
        if i = n then encoded
        else
            let pos = convert_char s.[i] in
            let encoded' = encoded lor (1 lsl (num_bits - pos - 1)) in
            loop (succ i) encoded'
    in
    loop 0 0
;;


(*
 * Num segments -> possible digits
 * 2 -> [ 1 ]
 * 3 -> [ 7 ]
 * 4 -> [ 4 ]
 * 5 -> [ 2; 3; 5 ]
 * 6 -> [ 6; 9 ]
 * 7 -> [ 8 ]
 *)


(** Return a mapping from input signal number to the segment that will actually be displayed (this
    mapping has to be reversed to decode the given "outputs").
   *)
let resolve signals =
    (* This will map from an integer to a mask. For example, A is index 0. If signal A maps to
       signal G, then [marks.(0) = 0b0000001]. *)
    let marks = Array.make 7 (-1) in
    let a_pos = convert_char 'a' in
    let b_pos = convert_char 'b' in
    let c_pos = convert_char 'c' in
    let d_pos = convert_char 'd' in
    let e_pos = convert_char 'e' in
    let f_pos = convert_char 'f' in
    let g_pos = convert_char 'g' in
    let one = array_find (fun x -> String.length x = 2) signals in
    let one_encoded = encode_string one in
    let seven = array_find (fun x -> String.length x = 3) signals in
    let seven_encoded = encode_string seven in
    let four = array_find (fun x -> String.length x = 4) signals in
    let four_encoded = encode_string four in
    (* Printf.printf "four=%s four_encoded=%x\n" four four_encoded; *)
    (* Ooops, what about zero? *)
    let nine = array_find (fun x ->
        String.length x = 6 && (encode_string x land four_encoded) = four_encoded) signals in
    let six = array_find (fun x ->
        String.length x = 6 && (encode_string x land one_encoded) <> one_encoded) signals in
    let nine_encoded = encode_string nine in
    let six_encoded = encode_string six in
    (* Printf.printf "six=%s six_encoded=%x\n" six six_encoded;
    Printf.printf "nine=%s nine_encoded=%x\n" nine nine_encoded; *)
    (* Printf.printf "(six_encoded lxor nine_encoded)=%x\n" (six_encoded lxor nine_encoded); *)
    (* Printf.printf "((six_encoded lxor nine_encoded) land one_encoded)=%x\n" ((six_encoded lxor
       nine_encoded) land one_encoded); *)
    let mask_c = (six_encoded lxor nine_encoded) land one_encoded in
    let mask_e = (six_encoded lxor nine_encoded) land (lnot one_encoded) in
    let mask_f = one_encoded lxor mask_c in
    let rec helper () =
        if Array.for_all (fun x -> x <> (-1)) marks then
            marks
        else
            (* Populate A *)
            if marks.(a_pos) = (-1) then
                (* If A is not marked, we have not found 1 and 7 *)
                let mask_a = one_encoded lxor seven_encoded in
                marks.(a_pos) <- mask_a;
                helper ()
            (* Populate C, E, and F *)
            else if marks.(c_pos) = (-1) then begin
                (* If C is not marked, we have not found 6 and 9 *)
                (* Find 9 by identifying the 6-segment set of signals that overlaps 1 *)
                marks.(c_pos) <- mask_c;
                marks.(e_pos) <- mask_e;
                marks.(f_pos) <- mask_f;
                (* Printf.printf "mask_c=%x\n" mask_c;
                Printf.printf "mask_e=%x\n" mask_e;
                Printf.printf "mask_f=%x\n" mask_f; *)
                helper ()
            end
            (* Populate B *)
            else if marks.(b_pos) = (-1) then
                (* If B is not marked, we have not found 2 and 5 *)
                let two = array_find (fun x ->
                    String.length x = 5 && (encode_string x land marks.(e_pos)) = marks.(e_pos)) signals
                in
                let five = array_find (fun x ->
                    String.length x = 5 && (encode_string x land marks.(c_pos)) <> marks.(c_pos)) signals
                in
                let two_encoded = encode_string two in
                let five_encoded = encode_string five in
                let mask_b = (two_encoded lxor five_encoded) land (lnot (one_encoded lor mask_e)) in
                (* Printf.printf "mask_e=%x\n" mask_e;
                Printf.printf "mask_b=%x\n" mask_b; *)
                marks.(b_pos) <- mask_b;
                helper ()
            (* Populate D and G *)
            else if marks.(d_pos) = (-1) then
                (* If D is not marked, we can figure it out from 4 *)
                let mask_d = (four_encoded land (lnot one_encoded)) lxor marks.(b_pos) in
                marks.(d_pos) <- mask_d;
                (* Then G is the only remaining position *)
                let mask_g = (nine_encoded lxor four_encoded) land (lnot seven_encoded) in
                (* Printf.printf "mask_g=%x\n" mask_g; *)
                marks.(g_pos) <- mask_g;
                helper ()
            else
                marks
    in
    let marks = helper () in
    (* Array.iter (fun mark ->
        Printf.printf "%x " mark
    ) marks;
    Printf.printf "\n"; *)
    (* pos 3 and 4 in marks have more than 1 bit set, corresponding to d and e *)
    marks
;;


exception Invalid_digit of int


let encode_int n =
    match n with
    | 0 -> 0b1110111
    | 1 -> 0b0010010
    | 2 -> 0b1011101
    | 3 -> 0b1011011
    | 4 -> 0b0111010
    | 5 -> 0b1101011
    | 6 -> 0b1101111
    | 7 -> 0b1010010
    | 8 -> 0b1111111
    | 9 -> 0b1111011
    | _ -> raise (Invalid_digit n)
;;


exception Invalid_signals of string


let decode_int n =
    match n with
    | 0b1110111 -> 0
    | 0b0010010 -> 1
    | 0b1011101 -> 2
    | 0b1011011 -> 3
    | 0b0111010 -> 4
    | 0b1101011 -> 5
    | 0b1101111 -> 6
    | 0b1010010 -> 7
    | 0b1111111 -> 8
    | 0b1111011 -> 9
    | _ -> raise (Invalid_signals (Printf.sprintf "%x" n))
;;

exception Invalid_mask of string

let mask_to_int m =
    match m with 
    | 0b1000000 -> 1
    | 0b0100000 -> 2
    | 0b0010000 -> 3
    | 0b0001000 -> 4
    | 0b0000100 -> 5
    | 0b0000010 -> 6
    | 0b0000001 -> 7
    | _ -> raise (Invalid_mask (Printf.sprintf "%x" m))
;;

let mask_to_char m =
    match m with 
    | 0b1000000 -> 'a'
    | 0b0100000 -> 'b'
    | 0b0010000 -> 'c'
    | 0b0001000 -> 'd'
    | 0b0000100 -> 'e'
    | 0b0000010 -> 'f'
    | 0b0000001 -> 'g'
    | _ -> raise (Invalid_mask (Printf.sprintf "%x" m))
;;

let decode_mask n =
    let num_bits = 7 in
    let get_mask i =
        1 lsl i
    in
    let rec helper i chars =
        let mask = get_mask i in
        if i = num_bits then
            chars
        else
            let chars' =
                if n land mask = mask then
                    ((mask_to_char mask) :: chars)
                else
                    chars
            in
            helper (succ i) chars'
    in
    helper 0 []
;;


(** Reverse the mapping represented by [a]. In other words, if [a.(i) = j], return an array where
    [a.(j) = i]. Assumes [a.(i) < Array.length a] for all [i] in [a].
   *)
let reverse_mapping a =
    let result = Array.make (Array.length a) 0 in
    Array.iteri (fun i -> fun x ->
        result.((mask_to_int x) - 1) <- i;
    ) a;
    result
;;


let print_mapping mapping =
    let num_bits = 7 in
    Array.iteri (fun i -> fun m' ->
        let m = 1 lsl (num_bits - i - 1) in
        Printf.printf "%x x> %x\n" m m';
        Printf.printf "%c -> %c\n" (mask_to_char m) (mask_to_char m')
    ) mapping;
;;


(*
Take a numerical combination of signals (an integer that is <= 0x7f) and turn it into a decoded set
of signals.
*)
let decode_signals lookup signals =
    (*
    For each bit at position i in signals, set the bit at position lookup.(i) in the output.
    *)
    let num_bits = 7 in
    let rec loop i decoded =
        if i = num_bits then decoded
        else
            let i_mask = 1 lsl (num_bits - i - 1) in
            (* let i_mask = 1 lsl i in *)
            let decoded' = 
                if signals land i_mask = i_mask then decoded lor (1 lsl (num_bits - lookup.(i) - 1))
                (* if signals land i_mask = i_mask then decoded lor (1 lsl lookup.(i)) *)
                else decoded
            in
            loop (succ i) decoded'
    in
    loop 0 0
;;


let do_game entries =
    Array.fold_left (fun acc -> fun entry ->
        let input, output = entry in
        (* let output_encoded = Array.map encode_string output in *)
        (* Printf.printf "input=%s\n" (format_array_of_string input);
        Printf.printf "output=%s\n" (format_array_of_string output); *)
        let forward_lookup = resolve input in
        let lookup = reverse_mapping forward_lookup in
        (* let lookup_char = Array.map mask_to_char lookup in *)
        (* For each display in output, turn it into an integer and look it up in lookup. OR the results
        together, and decode them. Collect the digits in a list *)
        (* Printf.printf "lookup=%s\n" (format_array lookup (Printf.sprintf "%x")); *)
        (* print_mapping lookup; *)
        let mixed_signals = Array.map encode_string output in
        (* Printf.printf "mixed_signals=";
        Array.iter (Printf.printf "%x ") mixed_signals;
        Printf.printf "\n"; *)
        let unmixed_signals = Array.map (decode_signals lookup) mixed_signals in
        (* Printf.printf "unmixed_signals=";
        Array.iter (Printf.printf "%x ") unmixed_signals;
        Printf.printf "\n"; *)
        let decoded_digits = Array.map (fun x -> decode_int x) unmixed_signals in
        let rec digits_to_num digits i =
            let n = Array.length digits in
            if i = n then
                0
            else
                let digit = digits.(n - i - 1) * (pow 10 i) in
                digit + digits_to_num digits (succ i)
        in
        let num = digits_to_num decoded_digits 0 in
        num + acc
    ) 0 entries
;;


let process_file filename =
    let inc = open_in filename in
    let entries = load_file inc in
    let sum = do_game entries in
    Printf.printf "filename=%s\n" filename;
    Printf.printf "result=%d\n" sum;

    Printf.printf "\n";
;;

(*
a -> d
b -> g
c -> b
d -> c
e -> a
f -> e
g -> f

cdfeb -> bceag => abceg
cdfeg -> dagfb => abdfg (this one is right--the mapping is reversed)
   *)


let () =
    process_file "8-test.input";
    process_file "8.input";
;;
