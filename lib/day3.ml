open Util

type symbol = Gear | Other
type cell = Num of int | Symbol of symbol | Empty

let cell_to_string cell =
  match cell with
  | Num i -> Printf.sprintf "%d" i
  | Symbol s -> ( match s with Gear -> "*" | Other -> "#")
  | Empty -> "."

let char_to_cell ch =
  match char_to_int ch with
  | Some i -> Num i
  | None -> (
      match ch with '.' -> Empty | '*' -> Symbol Gear | _ -> Symbol Other)

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

let filter_adjacent fn board x y =
  let positions =
    [
      (x + 1, y);
      (x + 1, y + 1);
      (x, y + 1);
      (x - 1, y + 1);
      (x - 1, y);
      (x - 1, y - 1);
      (x, y - 1);
      (x + 1, y - 1);
    ]
  in
  let filter board (x, y) = fn (get_cell board x y) in
  List.filter (filter board) positions

let symbol_is_adjacent board x y =
  match
    filter_adjacent
      (fun cell -> match cell with Symbol _ -> true | _ -> false)
      board x y
  with
  | [] -> false
  | _ -> true

let adjacent_nums board x y =
  let positions =
    filter_adjacent
      (fun cell -> match cell with Num _ -> true | _ -> false)
      board x y
  in

  let partial_sort = List.sort (fun (x, _) (u, _) -> x - u) positions in
  let sorted = List.sort (fun (_, y) (_, v) -> y - v) partial_sort in
  let rec collapse acc last arr =
    match arr with
    | [] -> acc
    | (x, y) :: rest -> (
        match acc with
        | [] -> collapse [ (x, y) ] (x, y) rest
        | _ ->
            let lastx, lasty = last in
            if lasty == y && abs (lastx - x) == 1 then collapse acc (x, y) rest
            else collapse ((x, y) :: acc) (x, y) rest)
  in
  collapse [] (0, 0) sorted

let expand_num board x y =
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

let get_valid_parts board =
  let num = ref 0 in
  let parts = ref [] in
  let valid = ref false in

  for y = 0 to Array.length board - 1 do
    for x = 0 to Array.length board.(y) - 1 do
      match board.(y).(x) with
      | Num n ->
          num := (!num * 10) + n;
          valid := !valid || symbol_is_adjacent board x y
      | _ ->
          if !num > 0 && !valid then parts := !num :: !parts;
          num := 0;
          valid := false
    done
  done;

  !parts

let find_gear_ratio board =
  let total = ref 0 in

  for y = 0 to Array.length board - 1 do
    for x = 0 to Array.length board.(y) - 1 do
      match board.(y).(x) with
      | Symbol s -> (
          match s with
          | Gear -> (
              match adjacent_nums board x y with
              | _ :: _ :: _ :: _ -> ()
              | (fx, fy) :: (sx, sy) :: _ ->
                  total :=
                    !total + (expand_num board fx fy * expand_num board sx sy)
              | _ -> ())
          | Other -> ())
      | _ -> ()
    done
  done;

  !total

let part1 input =
  match parse_board input with
  | Ok board ->
      get_valid_parts board |> List.fold_left ( + ) 0
      |> Printf.printf "Part1: %d\n"
  | Error e -> print_endline e

let part2 input =
  match parse_board input with
  | Ok board -> find_gear_ratio board |> Printf.printf "Part 2: %d\n"
  | Error e -> print_endline e
