open Utils;;

let nums = Array.of_list [
        "00100";
        "11110";
        "10110";
        "10111";
        "10101";
        "01111";
        "00111";
        "11100";
        "10000";
        "11001";
        "00010";
        "01010";
];;


let num_bits = 5;;

let () =
        let nums = Array.map binary_string_to_int nums in
        Array.sort compare nums;

        Array.iter (function n ->
                let n_str = int_to_binary_string n num_bits in
                Printf.printf "%d\t%x\t%s\n" n n n_str) nums;
        (* print_array nums; *)
        Printf.printf "\n";
;;
