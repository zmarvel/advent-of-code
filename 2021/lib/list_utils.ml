

let list_find_duplicate_opt ls =
    let ls = List.sort compare ls in
    let rec helper ls =
        match ls with
        | fst :: snd :: rst -> if fst = snd then Some(fst) else helper (snd :: rst)
        | _ -> None
    in
    helper ls
;;
