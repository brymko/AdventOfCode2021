let read_raw = 
    let file = "day1.input" in
    let lines = ref [] in 
    let chan = open_in file in 
    try
        while true; do
            lines := input_line chan  :: !lines
        done; !lines
    with End_of_file ->
        close_in chan;
        List.rev !lines

type foldexpr = { counter: int; last : int option}

let int_of_bool b = if b then 1 else 0

let do_fold expr cur =
    match expr.last with
    | None -> {expr with last = Some cur }
    | Some last -> {counter = expr.counter + int_of_bool (cur > last); last = Some cur}

let (>>) f g x = g (f x)

let rec map_slide3 f l = 
    match l with
    | a :: b :: c :: r -> f [a; b; c;] :: map_slide3 f (b :: c :: r)
    | _ -> []

let rec sum l =
    match l with 
    | [] -> 0
    | e :: r -> e + sum r

let main = 
    let res = read_raw |> List.map (String.trim >> int_of_string) |> List.fold_left do_fold { counter = 0; last = None } in
    string_of_int res.counter |> print_endline;

    let res = read_raw |> List.map (String.trim >> int_of_string) |> map_slide3 sum |> List.fold_left do_fold { counter = 0; last = None } in
    string_of_int res.counter |> print_endline;



