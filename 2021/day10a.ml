(*
--- Day 10: Syntax Scoring ---

You ask the submarine to determine the best route out of the deep-sea cave, but it only replies:

Syntax error in navigation subsystem on line: all of them

All of them?! The damage is worse than you thought. You bring up a copy of the navigation subsystem (your puzzle input).

The navigation subsystem syntax is made of several lines containing chunks. There are one or more chunks on each line, and chunks contain zero or more other chunks. Adjacent chunks are not separated by any delimiter; if one chunk stops, the next chunk (if any) can immediately start. Every chunk must open and close with one of four legal pairs of matching characters:

    If a chunk opens with (, it must close with ).
    If a chunk opens with [, it must close with ].
    If a chunk opens with {, it must close with }.
    If a chunk opens with <, it must close with >.

So, () is a legal chunk that contains no other chunks, as is []. More complex but valid chunks include ([]), {()()()}, <([{}])>, [<>({}){}[([])<>]], and even (((((((((()))))))))).

Some lines are incomplete, but others are corrupted. Find and discard the corrupted lines first.

A corrupted line is one where a chunk closes with the wrong character - that is, where the characters it opens and closes with do not form one of the four legal pairs listed above.

Examples of corrupted chunks include (], {()()()>, (((()))}, and <([]){()}[{}]). Such a chunk can appear anywhere within a line, and its presence causes the whole line to be considered corrupted.

For example, consider the following navigation subsystem:

[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]

Some of the lines aren't corrupted, just incomplete; you can ignore these lines for now. The remaining five lines are corrupted:

    {([(<{}[<>[]}>{[]{[(<()> - Expected ], but found } instead.
    [[<[([]))<([[{}[[()]]] - Expected ], but found ) instead.
    [{[{({}]{}}([{[{{{}}([] - Expected ), but found ] instead.
    [<(<(<(<{}))><([]([]() - Expected >, but found ) instead.
    <{([([[(<>()){}]>(<<{{ - Expected ], but found > instead.

Stop at the first incorrect closing character on each corrupted line.

Did you know that syntax checkers actually have contests to see who can get the high score for syntax errors in a file? It's true! To calculate the syntax error score for a line, take the first illegal character on the line and look it up in the following table:

    ): 3 points.
    ]: 57 points.
    }: 1197 points.
    >: 25137 points.

In the above example, an illegal ) was found twice (2*3 = 6 points), an illegal ] was found once (57 points), an illegal } was found once (1197 points), and an illegal > was found once (25137 points). So, the total syntax error score for this file is 6+57+1197+25137 = 26397 points!

Find the first illegal character in each corrupted line of the navigation subsystem. What is the total syntax error score for those errors?
*)


open Utils;;


(* Load the matrix into an array of strings *)
let load_file inc =
    (* Load the rows in reverse, then reverse it at the end *)
    let rec helper lines =
        match input_line_opt inc with
        | Some(line) ->
                helper (line :: lines)
        | None -> lines
    in
    array_reverse (Array.of_list (helper []))
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



let find_first_unmatched s =
    let rec loop i stack acc =
        if i = String.length s then
            acc
        else
            let c = s.[i] in
            match c with
            | '(' | '[' | '{' | '<' -> loop (succ i) (c :: stack) acc
            | ')' | ']' | '}' | '>' -> (
                    match stack with
                    | top :: rst ->
                            let acc' = if (matching_brace c) = top then acc else (c :: acc) in
                            loop (succ i) rst acc'
                    | _ -> loop (succ i) stack (c :: acc))
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


let get_score c =
    match c with
    | ')' -> 3
    | ']' -> 57
    | '}' -> 1197
    | '>' -> 25137
    | _ -> raise (Invalid_brace c)
;;


let do_game chunks =
    (* For each chunk, find the unmatched braces *)
    let unmatched_braces = Array.fold_left (fun acc -> fun chunk ->
        let c = find_first_unmatched chunk in
        c :: acc) [] chunks
    in
    let total_score = List.fold_left (fun acc -> fun unmatched ->
        match unmatched with
        | hd :: _ -> (get_score hd) + acc
        | _ -> acc) 0 unmatched_braces
    in
    total_score
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
