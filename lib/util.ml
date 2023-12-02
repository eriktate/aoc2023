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
