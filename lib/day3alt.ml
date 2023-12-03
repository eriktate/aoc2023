open Util

type part = Gear of int list | Other of int list
type cell = Num of int | Part of part | Empty

let cell_to_string cell =
  match cell with
  | Num i -> Printf.sprintf "%d" i
  | Part s -> ( match s with Gear _ -> "*" | Other _ -> "#")
  | Empty -> "."

let char_to_cell ch =
  match char_to_int ch with
  | Some i -> Num i
  | None -> (
      match ch with
      | '.' -> Empty
      | '*' -> Part (Gear [])
      | _ -> Part (Other []))

let parse_board input =
  let lines = get_lines input in
  let* x_length =
    match lines with
    | [] -> Error "tried to parse empty input"
    | hd :: _ -> Ok (String.length hd)
  in
  let y_length = List.length lines in
  (* coordinate system is y then x so that the inner arrays represent contiguous rows *)
  let init_board = Array.make_matrix y_length x_length Empty in

  let rec parse board x y input =
    match input with
    | [] -> board
    | ch :: rest -> (
        match ch with
        | '\n' when y + 1 < y_length -> parse board 0 (y + 1) rest
        | '\n' -> board
        | _ ->
            char_to_cell ch |> Array.set board.(y) x;
            parse board (x + 1) y rest)
  in
  Ok (parse init_board 0 0 (explode input))

(* safe board access that treats out of bounds as empty *)
let get_cell board x y =
  let board_h = Array.length board in
  let board_w = Array.length board.(0) in
  if x < 0 || x >= board_w || y < 0 || y >= board_h then Empty
  else board.(y).(x)

let expand_num board (x, y) =
  let row = board.(y) in
  let rec find_end row idx =
    if idx < Array.length row then
      match row.(idx) with Num _ -> find_end row (idx + 1) | _ -> idx - 1
    else Array.length row - 1
  in

  let rec build_number row idx =
    if idx >= 0 then
      match row.(idx) with
      | Num n -> (build_number row (idx - 1) * 10) + n
      | _ -> 0
    else 0
  in

  build_number row (find_end row x)

let find_adjacent_nums board x y =
  let top_row = [ (x - 1, y - 1); (x, y - 1); (x + 1, y - 1) ] in
  let bottom_row = [ (x - 1, y + 1); (x, y + 1); (x + 1, y + 1) ] in
  let mid_row = [ (x - 1, y); (x + 1, y) ] in

  let map_fn board (x, y) =
    match get_cell board x y with Num _ -> Some (x, y) | _ -> None
  in
  let check_line row =
    match List.map (map_fn board) row with
    | Some _ :: Some _ :: Some r :: _ -> [ r ]
    | Some l :: None :: Some r :: _ -> [ l; r ]
    | Some l :: None :: None :: _ -> [ l ]
    | None :: None :: Some r :: _ -> [ r ]
    | _ -> []
  in

  List.map (expand_num board)
    (List.concat
       [
         check_line top_row;
         check_line bottom_row;
         List.filter_map (map_fn board) mid_row;
       ])

let collect_parts board =
  let parts = ref [] in

  for y = 0 to Array.length board - 1 do
    for x = 0 to Array.length board.(y) - 1 do
      match board.(y).(x) with
      | Part s -> (
          let nums = find_adjacent_nums board x y in
          match s with
          | Gear _ -> parts := Gear nums :: !parts
          | Other _ -> parts := Other nums :: !parts)
      | _ -> ()
    done
  done;

  !parts

let part1 input =
  match parse_board input with
  | Ok board ->
      collect_parts board
      |> List.fold_left
           (fun acc part ->
             match part with
             | Gear nums -> acc + List.fold_left ( + ) 0 nums
             | Other nums -> acc + List.fold_left ( + ) 0 nums)
           0
      |> Printf.printf "Part1: %d\n"
  | Error e -> print_endline e

let part2 input =
  match parse_board input with
  | Ok board ->
      collect_parts board
      |> List.fold_left
           (fun acc part ->
             acc
             +
             match part with
             | Gear nums -> (
                 match nums with
                 | _ :: _ :: _ :: _ -> 0
                 | first :: second :: _ -> first * second
                 | _ -> 0)
             | Other _ -> 0)
           0
      |> Printf.printf "Part 2: %d\n"
  | Error e -> print_endline e
