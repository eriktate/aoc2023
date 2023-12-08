open Util

exception Err of string

module IntSet = Set.Make (Int)
module IntMap = Map.Make (Int)

type card = { id : int; winning_numbers : int list; given_numbers : int list }

let parse_id label =
  (* splitting on 'd' as a hack to avoid issues with padding spaces *)
  match String.split_on_char 'd' label with
  | _ :: id :: _ -> (
      match int_of_string_opt (String.trim id) with
      | Some n -> Ok n
      | None -> Error (Printf.sprintf "id was not a valid int"))
  | _ -> Error (Printf.sprintf "no id found in label: %s" label)

let parse_numbers nums =
  nums |> String.trim |> String.split_on_char ' '
  |> List.filter_map int_of_string_opt

let parse_card line =
  match String.split_on_char ':' line with
  | label :: numbers :: _ -> (
      let* id = parse_id label in
      match String.split_on_char '|' numbers with
      | winning_numbers :: given_numbers :: _ ->
          Ok
            {
              id;
              winning_numbers = parse_numbers winning_numbers;
              given_numbers = parse_numbers given_numbers;
            }
      | _ -> Error "not enough number sets")
  | _ -> Error "label and numbers sections not found"

let get_card_matches card =
  let winning_set = IntSet.of_list card.winning_numbers in
  let given_set = IntSet.of_list card.given_numbers in
  IntSet.inter winning_set given_set

let get_card_value card =
  match IntSet.cardinal (get_card_matches card) with
  | 0 -> 0
  | 1 -> 1
  | x -> pow 2 (x - 1)

let increment_map map k v =
  match IntMap.find_opt k map with
  | Some existing -> IntMap.add k (existing + v) map
  | None -> IntMap.add k v map

let rec expand_cards map cards =
  match cards with
  | [] -> map
  | hd :: rest ->
      let matches =
        min (IntSet.cardinal (get_card_matches hd)) (List.length rest)
      in
      let copies = some_or (IntMap.find_opt hd.id map) 1 in
      let rec inc_subsequent_cards map i =
        if i <= matches then
          inc_subsequent_cards (increment_map map (hd.id + i) copies) (i + 1)
        else map
      in
      expand_cards (inc_subsequent_cards map 1) rest

let print_card card =
  Printf.printf "%d: {%s} %d\n" card.id
    (String.concat ", "
       (List.map
          (fun n -> Printf.sprintf "%d" n)
          (IntSet.to_list (get_card_matches card))))
    (get_card_value card)

let parse_cards input =
  let lines = get_lines input in
  try
    Ok
      (List.map
         (fun line ->
           match parse_card line with
           | Ok card -> card
           | Error err -> raise (Err err))
         lines)
  with Err e -> Error e

let part1 input =
  match parse_cards input with
  | Ok cards ->
      (* List.iter print_card cards; *)
      let total =
        List.fold_left (fun acc card -> get_card_value card + acc) 0 cards
      in
      Printf.printf "Part1: %d\n" total
  | Error err -> Printf.eprintf "failed to parse cards: %s\n" err

let part2 input =
  match parse_cards input with
  | Ok cards ->
      let map =
        List.fold_left
          (fun acc card -> IntMap.add card.id 1 acc)
          IntMap.empty cards
      in
      let expanded = expand_cards map cards in
      let total_cards = IntMap.fold (fun _ v acc -> acc + v) expanded 0 in
      Printf.printf "Part2: %d\n" total_cards
  | Error err -> Printf.eprintf "failed to parse cards: %s\n" err
