let (>>) g f x = 
    f (g x)

let main () = 
    let chall = Core.In_channel.read_all "day2.input" |> String.split_on_char '\n' |> List.map (String.trim >> String.split_on_char ' ') in

    List.fold_left (fun (hpos, depth) line -> match (List.hd line, List.(rev line |> hd) |> int_of_string_opt) with
        | ("forward", Some i) -> (hpos + i, depth)
        | ("down", Some i) -> (hpos, depth + i)
        | ("up", Some i) -> (hpos, depth - i)
        | _ -> (hpos, depth)
    ) (0, 0)  chall
    |> (fun (a, b) -> a * b) |> string_of_int |> print_endline;

    
    List.fold_left (fun (hpos, depth, aim) line -> match (List.hd line, List.(rev line |> hd) |> int_of_string_opt) with
        | ("forward", Some i) -> (hpos + i, depth + aim * i, aim)
        | ("down", Some i) -> (hpos, depth, aim + i)
        | ("up", Some i) -> (hpos, depth, aim - i)
        | _ -> (hpos, depth, aim)
    ) (0, 0, 0)  chall
    |> (fun (a, b, _) -> a * b) |> string_of_int |> print_endline

