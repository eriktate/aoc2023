let read_input day =
  let input = open_in (Printf.sprintf "./inputs/day%d.txt" day) in
  let str = really_input_string input (in_channel_length input) in
  close_in input;
  str

let rec ensure_all pred l =
  match l with
  | [] -> true
  | hd :: rest -> if pred hd then ensure_all pred rest else false

let clean_empty_lines =
  List.filter (fun line -> match line with "" -> false | _ -> true)

let explode str = List.init (String.length str) (String.get str)

let char_to_int ch =
  let i = int_of_char ch - 48 in
  if i >= 0 && i < 10 then Some i else None

let get_lines input = String.split_on_char '\n' input |> clean_empty_lines

(* apparently this is the result monad...? *)
let ( let* ) x f = match x with Ok x -> f x | Error _ as e -> e

let transpose mat =
  let w = Array.length mat in
  let h = Array.length mat.(0) in
  let result = Array.make_matrix h w mat.(0).(0) in
  for x = 0 to w - 1 do
    for y = 0 to h - 1 do
      Array.set result.(y) x mat.(x).(y)
    done
  done;
  result

let rec pow x n =
  match n with
  | 0 -> 1
  | _ when n mod 2 == 0 ->
      let m = pow x (n / 2) in
      m * m
  | _ -> x * pow x (n - 1)

let some_or opt otherwise = match opt with Some v -> v | None -> otherwise
