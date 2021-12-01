let int_of_bool b = if b then 1 else 0
let sum = List.fold_left (+) 0
let (>>) g f x = f (g x)

let rec map_slide3 f = function
    | a :: b :: c :: r -> f [a; b; c;] :: map_slide3 f (b :: c :: r)
    | _ -> []

let rec fold_window2 f y = function
    | a :: b :: r -> fold_window2 f (f y a b) (b :: r)
    | _ -> y

let main () = 
    let chall = Core.In_channel.read_all "day1.input" |> String.split_on_char '\n' |> List.filter_map (String.trim >> int_of_string_opt) in

    chall 
    |> fold_window2 (fun ctr a b -> ctr + int_of_bool (a < b)) 0
    |> string_of_int
    |> print_endline;

    chall
    |> map_slide3 sum 
    |> fold_window2 (fun ctr a b -> ctr + int_of_bool (a < b)) 0
    |> string_of_int
    |> print_endline
