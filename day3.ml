let (>>) g f x = 
    f (g x)

let rec add_list a b =
    match List.nth_opt a 0 , List.nth_opt b 0 with
    | Some ea, Some eb -> ea + eb :: add_list (List.tl a) (List.tl b)
    | Some ea, None -> ea :: add_list (List.tl a) []
    | None, Some eb -> eb :: add_list [] (List.tl b)
    | None, None -> []

let print_list f lst =
  let rec print_elements = function
    | [] -> ()
    | h::t -> f h; print_string ";"; print_elements t
  in
  print_string "[";
  print_elements lst;
  print_endline "]";;

let filter_mask mask e =
    print_list print_char e;
    match e with 
    | a :: r when a = mask -> Some r
    | _ -> None

let rec part2 maskfn l = 
    let mask = maskfn l in 
    print_char mask;
    print_newline ();
    match List.filter_map (filter_mask mask) l with
    | [] -> List.hd l
    | a :: [] -> [mask] @ a
    | r -> mask :: part2 maskfn r

let gen_mask_ox l =
    let amount1 = List.fold_left (+) 0 (List.map (List.hd >> int_of_char >> (+) (-0x30)) l) in
    if amount1 * 2 >= List.length l then
        '1'
    else
        '0'

let gen_mask_co l =
    let amount1 = List.fold_left (+) 0 (List.map (List.hd >> int_of_char >> (+) (-0x30)) l) in
    if amount1 * 2 >= List.length l then
        '0'
    else 
        '1'

let main () = 
    let chall = Core.In_channel.read_all "day3.input" |> String.split_on_char '\n' |> List.map (String.trim) in

    chall
    |> List.map (Core.String.to_list >> (fun l -> List.map (int_of_char >> (+) (-0x30)) l))
    |> List.fold_left add_list []
    |> List.map (fun i -> if i - List.length chall / 2 > 0 then '1' else '0')
    |> Core.String.of_char_list 
    |> (fun s -> (s, String.map (fun c -> if c = '1' then '0' else '1') s))
    |> (fun (s1, s2) -> (int_of_string ("0b" ^ s1), int_of_string ("0b" ^ s2)))
    |> (fun (gamma, eps) -> gamma * eps)
    |> string_of_int |> print_endline;

    chall
    |> List.map Core.String.to_list
    |> List.filter (fun l -> List.length l > 0 )
    |> (fun c -> (part2 gen_mask_ox c, part2 gen_mask_co c))
    |> (fun (gamma, eps) ->
            (Core.String.of_char_list gamma |> (fun a -> "0b" ^ a) |> int_of_string)
            * (Core.String.of_char_list eps |> (fun a -> "0b" ^ a) |> int_of_string)
            )
    |> string_of_int |> print_endline;
