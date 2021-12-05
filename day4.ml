let (>>) g f x = 
    f (g x)
type board = { board : int list list } [@@deriving show]

let sum_board board = 
    List.map (List.fold_left (+) 0) board
    |> List.fold_left (+) 0

let element_in l e = List.mem e l

let sum_board_without board l = 
    List.map (List.filter (element_in l >> not) >> List.fold_left (+) 0) board
    |> List.fold_left (+) 0


let bingo_veritcal board l =
    List.exists (List.for_all (element_in l)) board

let rec bingo_horizontal board l =
    if List.for_all (fun l -> List.length l > 0) board then
        let bingo = List.map List.hd board
        |> List.for_all (element_in l) in

        if bingo then
            true
        else
            bingo_horizontal (List.map List.tl board) l
    else 
        false

let bingo l board = 
    (bingo_veritcal board.board l) || (bingo_horizontal board.board l)
    
let parse_seq c = String.split_on_char c >> List.filter_map (String.trim >> int_of_string_opt)
let parse_feed = parse_seq ','
let parse_board_seq = parse_seq ' '


let rec part1 boards drawn = function
    | [] -> raise Not_found
    | a :: r -> match List.find_opt (bingo (drawn @ [a])) boards with
        | None  -> part1 boards (drawn @ [a]) r
        | Some b -> 
                print_int (a * sum_board_without b.board (drawn @ [a]));
                print_newline ();
                ()

let rec part2 boards drawn = function
    | [] -> raise Not_found
    | a :: r -> match List.find_opt (bingo (drawn @ [a])) boards with
        | None  -> part2 boards (drawn @ [a]) r
        | Some b ->
            if List.length boards = 1 then (
                print_int (List.length boards);
                print_int (a * sum_board_without b.board (drawn @ [a]))
            )
            else 
                part2 (List.filter (bingo (drawn @ [a]) >> not) boards) (drawn @ [a]) r

let rec parse_board boards board = function
    | [] -> boards
    | "" :: r -> parse_board (boards @ [board]) {board = []} r
    | line :: r -> parse_board boards {board = (board.board @ [parse_board_seq line])} r

let main () =
    let chall = Core.In_channel.read_all "day4.input" |> String.split_on_char '\n' |> List.map (String.trim) in

    let sequence = parse_feed (List.hd chall) 
    and boards = List.tl (parse_board [] {board = []} (List.tl chall)) in

    part1 boards [] sequence;
    part2 boards [] sequence;


    print_newline ();
