(*
--- Part Two ---

Now that you have the structure of your transmission decoded, you can calculate the value of the expression it represents.

Literal values (type ID 4) represent a single number as described above. The remaining type IDs are more interesting:

    Packets with type ID 0 are sum packets - their value is the sum of the values of their sub-packets. If they only have a single sub-packet, their value is the value of the sub-packet.
    Packets with type ID 1 are product packets - their value is the result of multiplying together the values of their sub-packets. If they only have a single sub-packet, their value is the value of the sub-packet.
    Packets with type ID 2 are minimum packets - their value is the minimum of the values of their sub-packets.
    Packets with type ID 3 are maximum packets - their value is the maximum of the values of their sub-packets.
    Packets with type ID 5 are greater than packets - their value is 1 if the value of the first sub-packet is greater than the value of the second sub-packet; otherwise, their value is 0. These packets always have exactly two sub-packets.
    Packets with type ID 6 are less than packets - their value is 1 if the value of the first sub-packet is less than the value of the second sub-packet; otherwise, their value is 0. These packets always have exactly two sub-packets.
    Packets with type ID 7 are equal to packets - their value is 1 if the value of the first sub-packet is equal to the value of the second sub-packet; otherwise, their value is 0. These packets always have exactly two sub-packets.

Using these rules, you can now work out the value of the outermost packet in your BITS transmission.

For example:

    C200B40A82 finds the sum of 1 and 2, resulting in the value 3.
    04005AC33890 finds the product of 6 and 9, resulting in the value 54.
    880086C3E88112 finds the minimum of 7, 8, and 9, resulting in the value 7.
    CE00C43D881120 finds the maximum of 7, 8, and 9, resulting in the value 9.
    D8005AC2A8F0 produces 1, because 5 is less than 15.
    F600BC2D8F produces 0, because 5 is not greater than 15.
    9C005AC2F8F0 produces 0, because 5 is not equal to 15.
    9C0141080250320F1802104A08 produces 1, because 1 + 3 = 2 * 2.

What do you get if you evaluate the expression represented by your hexadecimal-encoded BITS transmission?
*)


open Utils;;


let hex_string_to_nums s =
    Array.init (String.length s / 2) (fun i ->
        let hi = ctoi_base 16 s.[2*i] in
        let lo = ctoi_base 16 s.[2*i+1] in
        (hi lsl 4) lor lo
    )
;;

let load_file inc =
    let encoded = input_line inc |> String.trim |> String.lowercase_ascii in
    let expected = input_line inc |> String.trim |> int_of_string in
    hex_string_to_nums encoded, expected
;;


type packet_header = { version : int; type_id : int; }
and operation = Sum | Product | Minimum | Maximum | Greater_than | Less_than | Equal_to
and packet =
        | Operator of {
                header : packet_header;
                length_type_id : int;
                operation: operation;
                length : int;
                children : packet list;
            }
        | Literal of {header : packet_header; value : int}
;;


let mask n_bits =
    (pow 2 n_bits) - 1
;;


let rec get_bits nums inum start_bit num_bits =
    let n = nums.(inum) in
    let num_bits_part = min (8 - start_bit) num_bits in
    let num_bits' = num_bits - num_bits_part in
    let shift = 8 - num_bits_part - start_bit in
    let n_mask = mask num_bits_part in
    let n_part = (n lsr shift) land (n_mask) in
    (*
    Printf.printf "  get_bits start=%d len=%d\n" (inum * 8 + start_bit) num_bits;
    Printf.printf "inum=%d start_bit=%d num_bits=%d\n" inum start_bit num_bits;
    Printf.printf "  num_bits_part=%d num_bits'=%d\n" num_bits_part num_bits';
    Printf.printf "  n=%x shift=%d n_mask=%x n_part=%x\n" n shift n_mask n_part;
    *)
    if num_bits' = 0 then n_part
    else
        let next = get_bits nums (succ inum) 0 num_bits' in
        (n_part lsl num_bits') lor (next)
;;


let assert_equal expected actual =
    if expected <> actual then
        Printf.printf "Not equal: expected=%d actual=%d\n" expected actual;
    assert (expected = actual);
;;


let split_num encoded =
    let rec loop parts encoded =
        if encoded = 0 then parts
        else loop ((encoded land 0xff) :: parts) (encoded lsr 8)
    in
    Array.of_list (loop [] encoded)
;;


let parse_header parts ipart ibit =
    let encoded_header = get_bits parts ipart ibit 6 in
    let version = encoded_header lsr 3 in
    let type_id = encoded_header land 0x7 in
    (*
    Printf.printf " header encoded=%x version=%d type_id=%d\n" encoded_header version type_id;
    *)
    { version; type_id }
;;


let add_bits ipart ibit len =
    let bit = (ipart * 8) + ibit + len in
    let ipart' = bit / 8  in
    let ibit' = bit mod 8 in
    ipart', ibit'
;;

let parse_literal header parts ipart ibit =
    (*
    Printf.printf " literal ipart=%d ibit=%d\n" ipart ibit;
    *)
    (* 0011 1000 0000 0000 0110 1111 0100 0101 0010 1001 0001 0010 0000 0000 *)
    let rec loop ipart ibit len num =
        let part = get_bits parts ipart ibit 5 in
        let ipart', ibit' = add_bits ipart ibit 5 in
        let len' = len + 5 in
        let num' = (num lsl 4) lor (part land 0x0f) in
        (*
        Printf.printf "ipart=%d ibit=%d num=%x part=%x ipart'=%d ibit'=%d\n" ipart ibit num part
        ipart' ibit';
        *)
        if (part land 0x10) = 0x10 then (* Keep reading numbers *)
            loop ipart' ibit' len' num'
        else
            len', num'
    in
    let len, value = loop ipart ibit 0 0 in
    (*
    Printf.printf "  len=%d value=%d\n" len value;
    *)
    len, Literal {header; value}
;;

exception Invalid_operation of int

let rec parse_packet encoded ipart ibit =
    (*
    Printf.printf "\n-----------------------------\n";
    Printf.printf "parse_packet ipart=%d ibit=%d\n" ipart ibit;
    *)
    let parse_operator header parts ipart ibit =
        let decode_operation id =
            match id with
            | 0 -> Sum
            | 1 -> Product
            | 2 -> Minimum
            | 3 -> Maximum
            | 5 -> Greater_than
            | 6 -> Less_than
            | 7 -> Equal_to
            | _ -> raise (Invalid_operation id)
        in

        let length_type_id = get_bits parts ipart ibit 1 in
        let ipart, ibit = add_bits ipart ibit 1 in
        (* Printf.printf "ipart=%d ibit=%d\n" ipart ibit; *)
        if length_type_id = 0 then
            let _ = () in
            (* Next 15 bits are total length of subpackets *)
            let len = get_bits parts ipart ibit 15 in
            (*
            Printf.printf " (operator0 len(bits)=%d ipart=%d ibit=%d\n" len ipart ibit;
            *)
            let ipart, ibit = add_bits ipart ibit 15 in
            let children_len, children =
                let rec parse_children0 i ipart ibit packets =
                    (*
                    Printf.printf "   (parse_children0 i=%d ipart=%d ibit=%d\n" i ipart ibit;
                    *)
                    if i = (len + 1 + 15) then begin
                        (*
                        Printf.printf "   )parse_children0 len=%d\n" i;
                        *)
                        i, List.rev packets
                    end
                    else
                        let packet_len, packet = parse_packet parts ipart ibit in
                        let ipart', ibit' = add_bits ipart ibit packet_len in
                        (*
                        Printf.printf "   )parse_children0 packet_len=%d\n" packet_len;
                        *)
                        parse_children0 (i + packet_len) ipart' ibit' (packet :: packets)
                in
                parse_children0 (1 + 15) ipart ibit []
            in
            let num_children = List.length children in
            (*
            Printf.printf " )operator0 children_len=%d\n" children_len;
            *)
            (* total length of children plus length type id plus children length*)
            (children_len, Operator {header; length_type_id; length = num_children;
                operation = decode_operation header.type_id; children})
        else
            (* Next 11 bits are the number of subpackets *)
            let _ = () in
            let num_children = get_bits parts ipart ibit 11 in
            (*
            Printf.printf " (operator1 ipart=%d ibit=%d num_children=%d\n" ipart ibit num_children;
            *)
            let ipart, ibit = add_bits ipart ibit 11 in
            let children_len, children =
                let rec parse_children1 i ipart ibit len packets =
                    (*
                    Printf.printf "   (parse_children1 i=%d ipart=%d ibit=%d len=%d\n" i ipart ibit len;
                    *)
                    if i = num_children then begin
                        (*
                        Printf.printf "   )parse_children1\n";
                        *)
                        len, List.rev packets
                    end
                    else
                        let packet_len, packet = parse_packet parts ipart ibit in
                        let ipart', ibit' = add_bits ipart ibit packet_len in
                        (*
                        Printf.printf "   )parse_children1 packet_len=%d\n" packet_len;
                        *)
                        parse_children1 (succ i) ipart' ibit' (len + packet_len) (packet :: packets)
                in
                parse_children1 0 ipart ibit (1 + 11) []
            in
            (*
            Printf.printf " )operator1 children_len=%d\n" children_len;
            *)
            (* total length of children plus length type id plus children length *)
            (children_len, Operator {header; length_type_id;
                operation = decode_operation header.type_id; length = num_children; children})
    in

    let header = parse_header encoded ipart ibit in
    let header_len = 6 in
    let ipart, ibit = add_bits ipart ibit header_len in
    let packet_len, packet =
        if header.type_id = 4 then parse_literal header encoded ipart ibit
        else parse_operator header encoded ipart ibit
    in
    (*
    Printf.printf "type=%d packet_len=%d len=%d ipart=%d ibit=%d\n" header.type_id packet_len
        (header_len + packet_len) ipart ibit;
        *)
    (header_len + packet_len), packet
;;


let rec sum_versions packet =
    match packet with
    | Operator o ->
            List.fold_left (fun acc -> fun child ->
                acc + (sum_versions child)
            ) o.header.version o.children
    | Literal l -> l.header.version
;;


let rec eval packet =
    let min x y = if x < y then x else y in
    let max x y = if x > y then x else y in
    let greater_than x y = if x > y then 1 else 0 in
    let less_than x y = if x < y then 1 else 0 in
    let equal_to x y = if x = y then 1 else 0 in

    match packet with
    | Operator o ->
            let children' = List.map eval o.children in
            (match o.operation with
                | Sum -> List.fold_left (+) 0 children'
                | Product -> List.fold_left ( * ) 1 children'
                | Minimum -> List.fold_left min (List.hd children') (List.tl children')
                | Maximum -> List.fold_left max (List.hd children') (List.tl children')
                | Greater_than | Less_than | Equal_to ->
                        let x = List.nth children' 0 in
                        let y = List.nth children' 1 in
                        (match o.operation with
                        | Greater_than -> greater_than
                        | Less_than -> less_than
                        | Equal_to -> equal_to
                        | _ -> raise (Invalid_operation o.header.type_id)) x y)
    | Literal l -> l.value
;;


let fail () = assert_equal 0 1;;

let process_file filename =
    let inc = open_in filename in
    Printf.printf "\n============================================================\n";
    Printf.printf "filename=%s\n" filename;

    let encoded, expected = load_file inc in
    Printf.printf "encoded=%s expected=%d\n" (format_array encoded (fun x -> Printf.sprintf "%02x" x)) expected;
    let _, parsed = parse_packet encoded 0 0 in
    let result = eval parsed in
    Printf.printf "result=%d\n" result;
    if expected <> -1 then
        assert_equal expected result;

    Printf.printf "\n";
;;


let () =
    process_file "16-test8.input";
    process_file "16-test9.input";
    process_file "16-test10.input";
    process_file "16-test11.input";
    process_file "16-test12.input";
    process_file "16-test13.input";
    process_file "16-test14.input";
    process_file "16-test15.input";

    (* 182133632621 too low *)
    process_file "16b.input";
    (*
    *)
;;
