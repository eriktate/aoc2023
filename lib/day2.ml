exception Err of string

type bound = { red : int; blue : int; green : int }
type block = Red of int | Green of int | Blue of int
type game = { id : int; rounds : block list list }

let rec ensure_all pred l =
  match l with
  | [] -> true
  | hd :: rest -> if pred hd then ensure_all pred rest else false

let clean_empty_lines =
  List.filter (fun line -> match line with "" -> false | _ -> true)

let take2 l =
  match l with
  | [] -> None
  | first :: rest -> (
      match rest with [] -> None | second :: _ -> Some (first, second))

let take3 l =
  match l with
  | [] -> None
  | first :: rest -> (
      match rest with
      | [] -> None
      | second :: rest -> (
          match rest with
          | [] -> None
          | third :: _ -> Some (first, second, third)))

let parse_round round =
  let raw_blocks = String.split_on_char ',' round in
  List.map
    (fun block ->
      match take3 (String.split_on_char ' ' block) with
      | Some (_, count, color) -> (
          match color with
          | "red" -> Red (int_of_string count)
          | "green" -> Green (int_of_string count)
          | "blue" -> Blue (int_of_string count)
          | _ -> raise (Err (Printf.sprintf "invalid color '%s'" color)))
      | None -> raise (Err "expected block but none was found"))
    raw_blocks

let parse_game game_raw =
  match take2 (String.split_on_char ':' game_raw) with
  | None -> raise (Err "invalid game input, no colon")
  | Some (prefix, rounds) -> (
      match take2 (String.split_on_char ' ' prefix) with
      | Some (_, game_id) ->
          {
            id = int_of_string game_id;
            rounds = List.map parse_round (String.split_on_char ';' rounds);
          }
      | None -> raise (Err "invalid game input, failed to parse game prefix"))

let validate_round bounds round =
  ensure_all
    (fun block ->
      match block with
      | Red count -> count <= bounds.red
      | Green count -> count <= bounds.green
      | Blue count -> count <= bounds.blue)
    round

let find_minimum_bounds game =
  List.fold_left
    (fun acc round ->
      match round with
      | Red count when count > acc.red -> { acc with red = count }
      | Green count when count > acc.green -> { acc with green = count }
      | Blue count when count > acc.blue -> { acc with blue = count }
      | _ -> acc)
    { red = 0; blue = 0; green = 0 }
    (List.flatten game.rounds)

let part1 input =
  let bounds = { red = 12; green = 13; blue = 14 } in
  let games =
    List.map parse_game (clean_empty_lines (String.split_on_char '\n' input))
  in
  let valid_games =
    List.filter
      (fun game -> ensure_all (validate_round bounds) game.rounds)
      games
  in
  Printf.printf "%d\n"
    (List.fold_left (fun acc game -> acc + game.id) 0 valid_games)

let part2 input =
  let games =
    List.map parse_game (clean_empty_lines (String.split_on_char '\n' input))
  in
  let min_bounds = List.map find_minimum_bounds games in
  let power =
    List.fold_left
      (fun acc bound -> (bound.red * bound.green * bound.blue) + acc)
      0 min_bounds
  in
  Printf.printf "%d\n" power
