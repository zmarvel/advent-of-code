(*
--- Day 16: Packet Decoder ---

As you leave the cave and reach open waters, you receive a transmission from the Elves back on the ship.

The transmission was sent using the Buoyancy Interchange Transmission System (BITS), a method of packing numeric expressions into a binary sequence. Your submarine's computer has saved the transmission in hexadecimal (your puzzle input).

The first step of decoding the message is to convert the hexadecimal representation into binary. Each character of hexadecimal corresponds to four bits of binary data:

0 = 0000
1 = 0001
2 = 0010
3 = 0011
4 = 0100
5 = 0101
6 = 0110
7 = 0111
8 = 1000
9 = 1001
A = 1010
B = 1011
C = 1100
D = 1101
E = 1110
F = 1111

The BITS transmission contains a single packet at its outermost layer which itself contains many other packets. The hexadecimal representation of this packet might encode a few extra 0 bits at the end; these are not part of the transmission and should be ignored.

Every packet begins with a standard header: the first three bits encode the packet version, and the next three bits encode the packet type ID. These two values are numbers; all numbers encoded in any packet are represented as binary with the most significant bit first. For example, a version encoded as the binary sequence 100 represents the number 4.

Packets with type ID 4 represent a literal value. Literal value packets encode a single binary number. To do this, the binary number is padded with leading zeroes until its length is a multiple of four bits, and then it is broken into groups of four bits. Each group is prefixed by a 1 bit except the last group, which is prefixed by a 0 bit. These groups of five bits immediately follow the packet header. For example, the hexadecimal string D2FE28 becomes:

110100101111111000101000
VVVTTTAAAAABBBBBCCCCC

Below each bit is a label indicating its purpose:

    The three bits labeled V (110) are the packet version, 6.
    The three bits labeled T (100) are the packet type ID, 4, which means the packet is a literal value.
    The five bits labeled A (10111) start with a 1 (not the last group, keep reading) and contain the first four bits of the number, 0111.
    The five bits labeled B (11110) start with a 1 (not the last group, keep reading) and contain four more bits of the number, 1110.
    The five bits labeled C (00101) start with a 0 (last group, end of packet) and contain the last four bits of the number, 0101.
    The three unlabeled 0 bits at the end are extra due to the hexadecimal representation and should be ignored.

So, this packet represents a literal value with binary representation 011111100101, which is 2021 in decimal.

Every other type of packet (any packet with a type ID other than 4) represent an operator that performs some calculation on one or more sub-packets contained within. Right now, the specific operations aren't important; focus on parsing the hierarchy of sub-packets.

An operator packet contains one or more packets. To indicate which subsequent binary data represents its sub-packets, an operator packet can use one of two modes indicated by the bit immediately after the packet header; this is called the length type ID:

    If the length type ID is 0, then the next 15 bits are a number that represents the total length in bits of the sub-packets contained by this packet.
    If the length type ID is 1, then the next 11 bits are a number that represents the number of sub-packets immediately contained by this packet.

Finally, after the length type ID bit and the 15-bit or 11-bit field, the sub-packets appear.

For example, here is an operator packet (hexadecimal string 38006F45291200) with length type ID 0 that contains two sub-packets:

00111000000000000110111101000101001010010001001000000000
VVVTTTILLLLLLLLLLLLLLLAAAAAAAAAAABBBBBBBBBBBBBBBB

    The three bits labeled V (001) are the packet version, 1.
    The three bits labeled T (110) are the packet type ID, 6, which means the packet is an operator.
    The bit labeled I (0) is the length type ID, which indicates that the length is a 15-bit number representing the number of bits in the sub-packets.
    The 15 bits labeled L (000000000011011) contain the length of the sub-packets in bits, 27.
    The 11 bits labeled A contain the first sub-packet, a literal value representing the number 10.
    The 16 bits labeled B contain the second sub-packet, a literal value representing the number 20.

After reading 11 and 16 bits of sub-packet data, the total length indicated in L (27) is reached, and so parsing of this packet stops.

As another example, here is an operator packet (hexadecimal string EE00D40C823060) with length type ID 1 that contains three sub-packets:

11101110000000001101010000001100100000100011000001100000
VVVTTTILLLLLLLLLLLAAAAAAAAAAABBBBBBBBBBBCCCCCCCCCCC

    The three bits labeled V (111) are the packet version, 7.
    The three bits labeled T (011) are the packet type ID, 3, which means the packet is an operator.
    The bit labeled I (1) is the length type ID, which indicates that the length is a 11-bit number representing the number of sub-packets.
    The 11 bits labeled L (00000000011) contain the number of sub-packets, 3.
    The 11 bits labeled A contain the first sub-packet, a literal value representing the number 1.
    The 11 bits labeled B contain the second sub-packet, a literal value representing the number 2.
    The 11 bits labeled C contain the third sub-packet, a literal value representing the number 3.

After reading 3 complete sub-packets, the number of sub-packets indicated in L (3) is reached, and so parsing of this packet stops.

For now, parse the hierarchy of the packets throughout the transmission and add up all of the version numbers.

Here are a few more examples of hexadecimal-encoded transmissions:

    8A004A801A8002F478 represents an operator packet (version 4) which contains an operator packet (version 1) which contains an operator packet (version 5) which contains a literal value (version 6); this packet has a version sum of 16.
    620080001611562C8802118E34 represents an operator packet (version 3) which contains two sub-packets; each sub-packet is an operator packet that contains two literal values. This packet has a version sum of 12.
    C0015000016115A2E0802F182340 has the same structure as the previous example, but the outermost packet uses a different length type ID. This packet has a version sum of 23.
    A0016C880162017C3686B18A3D4780 is an operator packet that contains an operator packet that contains an operator packet that contains five literal values; it has a version sum of 31.

Decode the structure of your hexadecimal-encoded BITS transmission; what do you get if you add up the version numbers in all packets?
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
    let rec helper s =
        match input_line_opt inc with
        | Some(line) -> helper (line |> String.trim |> String.lowercase_ascii)
        | None -> s
    in
    hex_string_to_nums (helper "")
;;


type packet_header = { version : int; type_id : int; }
and packet =
        | Operator of {
                header : packet_header;
                length_type_id : int;
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
        Printf.printf "Not equal: expected=%x actual=%x\n" expected actual;
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
    (* Printf.printf " header encoded=%x version=%d type_id=%d\n" encoded_header version type_id; *)
    { version; type_id }
;;


let add_bits ipart ibit len =
    let bit = (ipart * 8) + ibit + len in
    let ipart' = bit / 8  in
    let ibit' = bit mod 8 in
    ipart', ibit'
;;

let parse_literal header parts ipart ibit =
    Printf.printf " literal ipart=%d ibit=%d\n" ipart ibit;
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
    Printf.printf "  len=%d value=%d\n" len value;
    len, Literal {header; value}
;;


let rec parse_packet encoded ipart ibit =
    (*
    Printf.printf "\n-----------------------------\n";
    Printf.printf "parse_packet ipart=%d ibit=%d\n" ipart ibit;
    *)
    let parse_operator header parts ipart ibit =
        let length_type_id = get_bits parts ipart ibit 1 in
        let ipart, ibit = add_bits ipart ibit 1 in
        (* Printf.printf "ipart=%d ibit=%d\n" ipart ibit; *)
        if length_type_id = 0 then
            let _ = () in
            (* Next 15 bits are total length of subpackets *)
            let len = get_bits parts ipart ibit 15 in
            Printf.printf " (operator0 len(bits)=%d ipart=%d ibit=%d\n" len ipart ibit;
            let ipart, ibit = add_bits ipart ibit 15 in
            let children_len, children =
                let rec loop i ipart ibit packets =
                    if i = (len + 1 + 15) then len, List.rev packets
                    else
                        let packet_len, packet = parse_packet parts ipart ibit in
                        let ipart', ibit' = add_bits ipart ibit packet_len in
                        Printf.printf "   child0 packet_len=%d\n" packet_len;
                        loop (i + packet_len) ipart' ibit' (packet :: packets)
                in
                loop (1 + 15) ipart ibit []
            in
            let num_children = List.length children in
            Printf.printf " )operator0 children_len=%d\n" children_len;
            (* total length of children plus length type id plus children length*)
            (children_len, Operator {header; length_type_id; length = num_children; children})
        else
            (* Next 11 bits are the number of subpackets *)
            let _ = () in
            let num_children = get_bits parts ipart ibit 11 in
            Printf.printf " (operator1 ipart=%d ibit=%d num_children=%d\n" ipart ibit num_children;
            let ipart, ibit = add_bits ipart ibit 11 in
            let children_len, children =
                let rec loop i ipart ibit len packets =
                    (* Printf.printf "loop i=%d ipart=%d ibit=%d len=%d\n" i ipart ibit len; *)
                    if i = num_children then len, List.rev packets
                    else
                        let packet_len, packet = parse_packet parts ipart ibit in
                        let ipart', ibit' = add_bits ipart ibit packet_len in
                        Printf.printf "   child1 packet_len=%d\n" packet_len;
                        loop (succ i) ipart' ibit' (len + packet_len) (packet :: packets)
                in
                loop (1 + 11) ipart ibit 0 []
            in
            Printf.printf " )operator1 children_len=%d\n" children_len;
            (* total length of children plus length type id plus children length *)
            (children_len, Operator {header; length_type_id; length = num_children; children})
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


let fail () = assert_equal 0 1;;

let process_file filename =
    let inc = open_in filename in
    Printf.printf "\n============================================================\n";
    Printf.printf "filename=%s\n" filename;

    let encoded = load_file inc in
    Printf.printf "encoded=%s\n" (format_array encoded (fun x -> Printf.sprintf "%02x" x));

    if filename = "16-test1.input" then begin
        assert_equal (0xd2 land 0x7f) (get_bits encoded 0 1 7);
        assert_equal ((0xd2 lsr 2) land 0x7f) (get_bits encoded 0 0 6);
        assert_equal 0x2f (get_bits encoded 0 4 8);
        assert_equal ((0xfe28 lsr 2) land 0xff) (get_bits encoded 1 6 8);

        Array.iter2 (fun expected -> fun actual ->
            assert_equal expected actual
        ) [| 0x12; 0x34 |] (split_num 0x1234);

        (* Header len is 6 bits *)
        (let _, parsed = parse_literal {version = 0; type_id = 4} encoded 0 6 in
        match parsed with
        | Operator _ -> assert_equal 0 1
        | Literal literal -> assert_equal 2021 literal.value);
    end
    else if filename = "16-test2.input" then begin
        (* 16-test2
        assert_equal 0b11011 (get_bits encoded 0 7 15);
        *)
        (*
0        1        2        3        4        5        6
00111000 00000000 01101111 01000101 00101001 00010010 00000000
VVVTTTIL LLLLLLLL LLLLLLAA AAAAAAAA ABBBBBBB BBBBBBBB B
        *)

        let _, parsed = parse_packet encoded 0 0 in
        match parsed with
        | Operator o -> begin
            assert_equal 2 o.length;
            (match List.nth o.children 0 with
            | Literal literal -> assert_equal 10 literal.value
            | _ -> fail ());
            (match List.nth o.children 1 with
            | Literal literal -> assert_equal 20 literal.value
            | _ -> fail ());
        end
        | _ -> fail ()
    end
    else if filename = "16-test3.input" then begin
        let _, parsed = parse_packet encoded 0 0 in
        match parsed with
        | Operator o -> begin
                assert_equal 3 o.length;
                (match List.nth o.children 0 with
                | Literal l -> assert_equal 1 l.value
                | _ -> fail ()
                );
                (match List.nth o.children 1 with
                | Literal l -> assert_equal 2 l.value
                | _ -> fail ()
                );
                (match List.nth o.children 2 with
                | Literal l -> assert_equal 3 l.value
                | _ -> fail ()
                );
        end
        | _ -> fail ();
    end
    else
        let _, parsed = parse_packet encoded 0 0 in
        let expected =
            match filename with
            | "16-test4.input" -> 16
            | "16-test5.input" -> 12
            | "16-test6.input" -> 23
            | "16-test7.input" -> 31
            | _ -> -1
        in
        assert_equal expected (sum_versions parsed);

    Printf.printf "\n";
;;


let () =
    begin
        let ipart, ibit = add_bits 0 7 15 in
        assert_equal 2 ipart;
        assert_equal 6 ibit;
    end;

    process_file "16-test1.input";
    (* 2
00111000 00000000 01101111 01000101 00101001 00010010 00000000
VVVTTTIL LLLLLLLL LLLLLLAA AAAAAAAA ABBBBBBB BBBBBBBB B
operator0 <27>
                        len(A)=11    len(B)=16

    *)
    process_file "16-test2.input";
    process_file "16-test3.input";


    process_file "16-test4.input";
    (* 5
620080001611562C8802118E34
0        1        2        3        4        5        6        7        8        9        10       11       12
01100010 00000000 10000000 00000000 00010110 00010001 01010110 00101100 10001000 00000010 00010001 10001110 00110100
operator1 <2>                                                        operator1 <2>
VVVTTTIL LLLLLLLL LL                                                 VV VTTTILLL LLLLLLLL
                    operator0 <22>                 0xa          0xb                             0xc          0xd
                    VVVTTT ILLLLLLL LLLLLLLL|00010001 01010110 001011|                    VVVTTTAA AAAVVVTT TAAAAA
                                             VVVTTTAA AAAVVVTT TAAAAA
                                             11111111 11122222 222222
                    6+1+15+11+11=22+22=44
       *)
    process_file "16-test5.input";
    process_file "16-test6.input";
    process_file "16-test7.input";

    (*
    process_file "16.input";
    *)
;;
