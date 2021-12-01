type foldexpr = { counter: int; last : int option}

let int_of_bool b = if b then 1 else 0

let do_fold expr cur =
    match expr.last with
    | None -> {expr with last = Some cur }
    | Some last -> {counter = expr.counter + int_of_bool (cur > last); last = Some cur}

let (>>) f g x = g (f x)

let rec map_slide3 f = function
    | a :: b :: c :: r -> f [a; b; c;] :: map_slide3 f (b :: c :: r)
    | _ -> []

let sum = List.fold_left (+) 0

let grab_counter x = x.counter

let main () = 
    let chall = Core.In_channel.read_all "day1.input" |> String.split_on_char '\n' |> List.filter_map (String.trim >> int_of_string_opt) in

    chall
    |> List.fold_left do_fold { counter = 0; last = None }
    |> grab_counter
    |> string_of_int
    |> print_endline;

    chall
    |> map_slide3 sum 
    |> List.fold_left do_fold { counter = 0; last = None }
    |> grab_counter
    |> string_of_int
    |> print_endline



