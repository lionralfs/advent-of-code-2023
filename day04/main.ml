open Utils
module IntSet = Set.Make (Int)

let ic = stdin

type card = { _id : int; winning : IntSet.t; have : IntSet.t }

let rec build_list l line_parsing_fn =
  match input_line ic with
  | line -> build_list (line_parsing_fn line :: l) line_parsing_fn
  | exception End_of_file ->
      close_in ic;
      List.rev l

let parse_line line =
  let re = Str.regexp {|Card +\([0-9]+\):\(.*\)|\(.*\)|} in
  let result =
    if Str.string_match re line 0 then (
      let card_num = int_of_string (Str.matched_group 1 line) in
      let winning = Str.matched_group 2 line in
      let have = Str.matched_group 3 line in
      let winning_parts =
        List.filter
          (fun s -> String.length s > 0)
          (String.split_on_char ' ' winning)
      in
      Printf.printf "%d" (List.length winning_parts);
      let winning_parsed =
        List.map (fun part -> int_of_string (String.trim part)) winning_parts
      in
      let winning = IntSet.of_list winning_parsed in
      let have_parts =
        List.filter
          (fun s -> String.length s > 0)
          (String.split_on_char ' ' have)
      in
      let have_parsed =
        List.map (fun part -> int_of_string (String.trim part)) have_parts
      in
      let have = IntSet.of_list have_parsed in
      { _id = card_num; winning; have })
    else raise (Failure ("Unable to parse line" ^ line))
  in
  result

let matching_numbers card =
  IntSet.cardinal (IntSet.inter card.have card.winning)

let part_1 lines =
  let points =
    List.map
      (fun line ->
        let matches = matching_numbers line in
        (* wtf no pow in ocaml? *)
        match matches with
        | 1 -> 1
        | 2 -> 2
        | 3 -> 4
        | 4 -> 8
        | 5 -> 16
        | 6 -> 32
        | 7 -> 64
        | 8 -> 128
        | 9 -> 256
        | 10 -> 512
        | _ -> 0)
      lines
  in
  list_sum points 0

let part_2 _ = 0

let _ =
  let input = build_list [] parse_line in
  Printf.printf "[Part1]: %d\n" (part_1 input);
  Printf.printf "[Part2]: %d\n" (part_2 input)
