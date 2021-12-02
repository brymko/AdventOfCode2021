type command =
    | Forward of int
    | Down of int
    | Up of int

let command_of_string_list = function
    | a :: b :: _ -> (match a with
        | "forward" -> Some (Forward (int_of_string b))
        | "down" -> Some (Down (int_of_string b))
        | "up" -> Some (Up (int_of_string b))
        | _ -> None)
    | _ -> None

let convert line = 
    line |> String.split_on_char ' ' |> List.map String.trim |> command_of_string_list


let main () = 
    let chall = Core.In_channel.read_all "day2.input" |> String.split_on_char '\n' |> List.map String.trim |> List.filter_map convert in 

    List.fold_left (fun (hpos, depth) e -> match e with
        | Forward i -> (hpos + i, depth)
        | Down i -> (hpos, depth + i)
        | Up i -> (hpos, depth - i)
    ) (0, 0) chall
    |> (fun (a, b) -> a * b) |> string_of_int |> print_endline;

    List.fold_left (fun (hpos, depth, aim) e -> match e with
        | Forward i -> (hpos + i, depth + aim * i, aim)
        | Down i -> (hpos, depth, aim + i)
        | Up i -> (hpos, depth, aim - i)
    ) (0, 0, 0) chall
    |> (fun (a, b, _) -> a * b) |> string_of_int |> prerr_endline

    
