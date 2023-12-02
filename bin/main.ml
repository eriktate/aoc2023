open Aoc23

let read_input day =
  let input = open_in (Printf.sprintf "./inputs/day%d.txt" day) in
  let str = really_input_string input (in_channel_length input) in
  close_in input;
  str

let () = Day2.part2 (read_input 2)
