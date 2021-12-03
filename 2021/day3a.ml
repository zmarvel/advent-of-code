(*
--- Day 3: Binary Diagnostic ---

The submarine has been making some odd creaking noises, so you ask it to produce a diagnostic report just in case.

The diagnostic report (your puzzle input) consists of a list of binary numbers which, when decoded properly, can tell you many useful things about the conditions of the submarine. The first parameter to check is the power consumption.

You need to use the binary numbers in the diagnostic report to generate two new binary numbers (called the gamma rate and the epsilon rate). The power consumption can then be found by multiplying the gamma rate by the epsilon rate.

Each bit in the gamma rate can be determined by finding the most common bit in the corresponding position of all numbers in the diagnostic report. For example, given the following diagnostic report:

00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010

Considering only the first bit of each number, there are five 0 bits and seven 1 bits. Since the most common bit is 1, the first bit of the gamma rate is 1.

The most common second bit of the numbers in the diagnostic report is 0, so the second bit of the gamma rate is 0.

The most common value of the third, fourth, and fifth bits are 1, 1, and 0, respectively, and so the final three bits of the gamma rate are 110.

So, the gamma rate is the binary number 10110, or 22 in decimal.

The epsilon rate is calculated in a similar way; rather than use the most common bit, the least common bit from each position is used. So, the epsilon rate is 01001, or 9 in decimal. Multiplying the gamma rate (22) by the epsilon rate (9) produces the power consumption, 198.

Use the binary numbers in your diagnostic report to calculate the gamma rate and epsilon rate, then multiply them together. What is the power consumption of the submarine? (Be sure to represent your answer in decimal, not binary.)
 *)


let input_line_opt inc =
    try
        let line = input_line inc in
        Some(line)
    with
    | End_of_file -> None
;;


let add_arrays a1 a2 =
    (* Just assume they're the same length *)
    let out = Array.make (Array.length a1) 0 in
    Array.iteri (fun i x -> Array.set out i ((Array.get a2 i) + x)) a1;
    out
;;



(* Produces an array representing a mask m, where m[i] is true when a[i] > threshold
 *)
let array_greater_than threshold a =
    Array.map (fun x -> if x > threshold then 1 else 0) a
;;


(* Values greater than 0 are set to 1 and everything else is set to 0.
   *)
let array_to_binary =
    array_greater_than 0
;;


(* Values equal to 0 are set to 1 and everything else is set to 0.
   *)
let invert_array a =
    Array.map (fun x -> if x == 0 then 1 else 0) a
;;


(* Array of int from string *)
let array_from_string s =
    let a = Array.make (String.length s) 0 in
    String.iteri (fun i x -> Array.set a i ((int_of_char x) - (int_of_char '0'))) s;
    a
;;


(* From https://stackoverflow.com/questions/16950687/integer-exponentiation-in-ocaml *)
let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n -> 
    let b = pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a)
;;

let binary_to_int a =
    let rec loop i n =
        if i == Array.length a then
            n
        else
            loop (i + 1) (n + (Array.get a i) * (pow 2 ((Array.length a) - i - 1)))
    in
    loop 0 0
;;

let sum_readings inc array_size =
    let sum = Array.make array_size 0 in
    let rec loop sum count =
        match (input_line_opt inc) with
        | Some(line) ->
                let line = String.trim line in
                loop (add_arrays sum (array_from_string line)) (count + 1)
        | None ->
                (sum, count)
    in
    loop sum 0
;;


let print_array a =
    Printf.printf "[ ";
    Array.iter (Printf.printf "%d ") a;
    Printf.printf "]";
;;


let process_file filename array_size =
    Printf.printf "filename=%s\n" filename;
    let inc = open_in filename in
    let sum, count = sum_readings inc array_size in
    let gamma_rate = array_greater_than (count / 2) sum in
    let epsilon_rate = invert_array gamma_rate in
    Printf.printf "sum=";
    print_array sum;
    Printf.printf "\n";

    let gamma_int = (binary_to_int gamma_rate) in
    let epsilon_int = (binary_to_int epsilon_rate) in
    Printf.printf "gamma=%d=" gamma_int;
    print_array gamma_rate;
    Printf.printf "\n";

    Printf.printf "epsilon=%d=" epsilon_int;
    print_array epsilon_rate;
    Printf.printf "\n";

    Printf.printf "result=%d\n\n" (gamma_int * epsilon_int);
;;


let () =
    process_file "3-test.input" 5;
    process_file "3.input" 12;
;;
