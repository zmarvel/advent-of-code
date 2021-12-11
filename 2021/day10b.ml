(*
--- Part Two ---

Now, discard the corrupted lines. The remaining lines are incomplete.

Incomplete lines don't have any incorrect characters - instead, they're missing some closing characters at the end of the line. To repair the navigation subsystem, you just need to figure out the sequence of closing characters that complete all open chunks in the line.

You can only use closing characters (), ], }, or >), and you must add them in the correct order so that only legal pairs are formed and all chunks end up closed.

In the example above, there are five incomplete lines:

    [({(<(())[]>[[{[]{<()<>> - Complete by adding }}]])})].
    [(()[<>])]({[<{<<[]>>( - Complete by adding )}>]}).
    (((({<>}<{<{<>}{[]{[]{} - Complete by adding }}>}>)))).
    {<[[]]>}<{[{[{[]{()[[[] - Complete by adding ]]}}]}]}>.
    <{([{{}}[<[[[<>{}]]]>[]] - Complete by adding ])}>.

Did you know that autocomplete tools also have contests? It's true! The score is determined by considering the completion string character-by-character. Start with a total score of 0. Then, for each character, multiply the total score by 5 and then increase the total score by the point value given for the character in the following table:

    ): 1 point.
    ]: 2 points.
    }: 3 points.
    >: 4 points.

So, the last completion string above - ])}> - would be scored as follows:

    Start with a total score of 0.
    Multiply the total score by 5 to get 0, then add the value of ] (2) to get a new total score of 2.
    Multiply the total score by 5 to get 10, then add the value of ) (1) to get a new total score of 11.
    Multiply the total score by 5 to get 55, then add the value of } (3) to get a new total score of 58.
    Multiply the total score by 5 to get 290, then add the value of > (4) to get a new total score of 294.

The five lines' completion strings have total scores as follows:

    }}]])})] - 288957 total points.
    )}>]}) - 5566 total points.
    }}>}>)))) - 1480781 total points.
    ]]}}]}]}> - 995444 total points.
    ])}> - 294 total points.

Autocomplete tools are an odd bunch: the winner is found by sorting all of the scores and then taking the middle score. (There will always be an odd number of scores to consider.) In this example, the middle score is 288957 because there are the same number of scores smaller and larger than it.

Find the completion string for each incomplete line, score the completion strings, and sort the scores. What is the middle score?
*)


open Utils;;


(* Load the matrix into a list of strings *)
let load_file inc =
    let rec helper lines =
        match input_line_opt inc with
        | Some(line) ->
                helper (line :: lines)
        | None -> lines
    in
    List.rev (helper [])
;;


exception Invalid_brace of char

let matching_brace = function
    | '(' -> ')'
    | '[' -> ']'
    | '{' -> '}'
    | '<' -> '>'
    | ')' -> '('
    | ']' -> '['
    | '}' -> '{'
    | '>' -> '<'
    | c -> raise (Invalid_brace c)
;;


let is_chunk_corrupt s =
    let rec loop i stack =
        if i = String.length s then
            false
        else
            let c = s.[i] in
            match c with
            | '(' | '[' | '{' | '<' -> loop (succ i) (c :: stack)
            | ')' | ']' | '}' | '>' -> (
                    match stack with
                    | top :: rst ->
                            (* If the brace is not matched by the expected one, the string is
                               corrupt *)
                            if matching_brace c = top then
                                loop (succ i) rst
                            else
                                true
                    | _ ->
                            (* In this case, the chunk is incomplete, not corrupt *)
                            loop (succ i) stack)
            | _ -> raise (Invalid_brace c)
    in
    loop 0 []
;;


let filter_corrupt_chunks chunks =
    List.filter (fun chunk -> not (is_chunk_corrupt chunk)) chunks
;;


let complete_pairs s =
    let rec loop i stack acc =
        if i = String.length s then
            List.map matching_brace stack
        else
            let c = s.[i] in
            match c with
            | '(' | '[' | '{' | '<' -> loop (succ i) (c :: stack) acc
            | ')' | ']' | '}' | '>' -> (
                    match stack with
                    | _ :: rst ->
                            (* Assume this chunk has already been identified as incomplete, not
                               corrupt--so either the braces match, or there are missing closing
                               braces *)
                            loop (succ i) rst acc
                    | _ -> raise (Invalid_brace c))
            | _ -> raise (Invalid_brace c)
    in
    loop 0 [] []
;;


(** Make a string out of a list of char *)
let join_list_of_char (ls : char list) : string =
    let chars = Array.of_list ls in
    let n = Array.length chars in
    String.init n (Array.get chars)
;;



let print_chunks label chunks =
    List.iter (Printf.printf "%s=%s\n" label) chunks
;;


(*
Incomplete? (Include?)
Y [({(<(())[]>[[{[]{<()<>>
Y [(()[<>])]({[<{<<[]>>(
N {([(<{}[<>[]}>{[]{[(<()>
Y (((({<>}<{<{<>}{[]{[]{}
N [[<[([]))<([[{}[[()]]]
N [{[{({}]{}}([{[{{{}}([]
Y {<[[]]>}<{[{[{[]{()[[[]
N [<(<(<(<{}))><([]([]()
N <{([([[(<>()){}]>(<<{{
Y <{([{{}}[<[[[<>{}]]]>[]]
   *)


let get_score c =
    match c with
    | ')' -> 1
    | ']' -> 2
    | '}' -> 3
    | '>' -> 4
    | _ -> raise (Invalid_brace c)
;;


let do_game chunks =
    (* For each chunk, find the unmatched braces *)
    let incomplete_chunks = filter_corrupt_chunks chunks in
    let missing_braces = List.map complete_pairs incomplete_chunks in
    List.iter2 (fun chunk -> fun braces ->
        Printf.printf "chunk=%s missing=%s\n" chunk (join_list_of_char braces)
    ) incomplete_chunks missing_braces;


    let rec calc_score score braces =
        match braces with
        | [] -> score
        | hd :: rst -> calc_score ((5 * score) + (get_score hd)) rst
    in
    let scores = Array.of_list (List.map (calc_score 0) missing_braces) in
    Array.sort compare scores;
    scores.((Array.length scores) / 2)
;;


let process_file filename =
    let inc = open_in filename in
    let chunks = load_file inc in
    Printf.printf "filename=%s\n" filename;
    let result = do_game chunks in
    Printf.printf "result=%d\n" result;

    Printf.printf "\n";
;;

let () =
    process_file "10-test.input";
    process_file "10.input";
;;
