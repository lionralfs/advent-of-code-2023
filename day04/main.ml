open Utils
module IntSet = Set.Make (Int)
module IntMap = Map.Make (Int)

let ic = stdin

type card = { id : int; winning : IntSet.t; have : IntSet.t }

let rec build_list l line_parsing_fn =
  match input_line ic with
  | line -> build_list (line_parsing_fn line :: l) line_parsing_fn
  | exception End_of_file ->
      close_in ic;
      List.rev l

let parse_line line =
  let re = Str.regexp {|Card +\([0-9]+\):\(.*\)|\(.*\)|} in
  let result =
    if Str.string_match re line 0 then
      let card_num = int_of_string (Str.matched_group 1 line) in
      let winning = Str.matched_group 2 line in
      let have = Str.matched_group 3 line in
      let winning_parts =
        List.filter
          (fun s -> String.length s > 0)
          (String.split_on_char ' ' winning)
      in
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
      { id = card_num; winning; have }
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

let rec part_2_helper all_cards todo result_map =
  match todo with
  | [] -> result_map
  | head :: tail ->
      let card = all_cards |> List.find (fun card -> card.id == head) in
      let current_card_count = IntMap.find head result_map in
      let matching = matching_numbers card in
      let copies = range (head + 1) (head + matching) in
      let new_result =
        List.fold_left
          (fun acc copy ->
            IntMap.update copy
              (fun old ->
                match old with
                | Some old_value -> Some (old_value + current_card_count)
                | _ -> raise (Failure "impossible"))
              acc)
          result_map copies
      in
      part_2_helper all_cards tail new_result

let part_2 cards =
  let all_card_numbers = range 1 (List.length cards) in
  let initial = all_card_numbers |> List.map (fun num -> (num, 1)) in
  let start_map = IntMap.of_list initial in
  let todo = all_card_numbers in
  let result = part_2_helper cards todo start_map in
  let values = IntMap.bindings result |> List.split |> snd in
  list_sum values 0

let _ =
  let input = build_list [] parse_line in
  Printf.printf "[Part1]: %d\n" (part_1 input);
  Printf.printf "[Part2]: %d\n" (part_2 input)
