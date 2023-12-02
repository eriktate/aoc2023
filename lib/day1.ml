let explode str = List.init (String.length str) (String.get str)

let to_number ch =
  let code = int_of_char ch in
  if code > 48 && code <= 57 then Some (code - 48) else None

let parse_line input =
  let chars = explode input in
  let nums =
    List.fold_left
      (fun acc char ->
        match to_number char with Some n -> n :: acc | None -> acc)
      [] chars
  in
  (* list will be inverted because we basically unshift instead of push *)
  (List.rev nums |> List.hd, List.hd nums)

let read_string_digit str =
  match str with
  | "one" -> Some 1
  | "two" -> Some 2
  | "three" -> Some 3
  | "four" -> Some 4
  | "five" -> Some 5
  | "six" -> Some 6
  | "seven" -> Some 7
  | "eight" -> Some 8
  | "nine" -> Some 9
  | _ -> None

let rec find_number rev words input =
  match input with
  | [] -> 0
  | ch :: rest -> (
      match to_number ch with
      | Some num -> num
      | None -> (
          let words =
            Char.escaped ch
            :: List.map
                 (fun word ->
                   if rev then Char.escaped ch ^ word
                   else word ^ Char.escaped ch)
                 words
          in
          let found =
            List.find_map (fun word -> read_string_digit word) words
          in
          match found with
          | Some num -> num
          | None -> find_number rev words rest))

let parse_line2 input =
  ( find_number false [] (explode input),
    find_number true [] (explode input |> List.rev) )

let clean_empty_lines =
  List.filter (fun line -> match line with "" -> false | _ -> true)

let render_result pairs =
  let totals =
    List.map
      (fun pair ->
        let first, last = pair in
        (first * 10) + last)
      pairs
  in
  let answer = List.fold_left (fun acc total -> acc + total) 0 totals in
  Printf.printf "%d\n" answer

let part1 input =
  let pairs =
    List.map parse_line (clean_empty_lines (String.split_on_char '\n' input))
  in
  render_result pairs

let part2 input =
  let pairs =
    List.map parse_line2 (clean_empty_lines (String.split_on_char '\n' input))
  in
  render_result pairs
