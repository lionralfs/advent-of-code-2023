open Utils

let ic = stdin

module StringMap = Map.Make (String)

type draw = int StringMap.t
type game = { id : int; draws : draw list }

let parse_draw_part part =
  let trimmed = String.trim part in
  Scanf.sscanf trimmed "%d %s" (fun a b -> (a, b))

let parse_draw draw =
  let trimmed = String.trim draw in
  let parts = String.split_on_char ',' trimmed in
  List.fold_left
    (fun acc curr ->
      let n, color = parse_draw_part curr in
      StringMap.add color n acc)
    StringMap.empty parts

let parse_line_parts game_id rest =
  let draws_raw = String.split_on_char ';' rest in
  let draws = List.map parse_draw draws_raw in
  { id = game_id; draws }

let parse_line line = Scanf.sscanf line "Game %d: %s@\t" parse_line_parts

let rec build_list l line_parsing_fn =
  match input_line ic with
  | line -> build_list (line_parsing_fn line :: l) line_parsing_fn
  | exception End_of_file ->
      close_in ic;
      List.rev l

let max_red = 12
let max_green = 13
let max_blue = 14

let exeeds target color game =
  let draws = game.draws in
  let sum =
    List.map
      (fun draw ->
        match StringMap.find_opt color draw with
        | Some value -> value
        | None -> 0)
      draws
  in
  let exeeding = List.exists (fun sum -> sum > target) sum in
  exeeding

let part_1 game_list =
  let remaining_games =
    List.filter
      (fun game ->
        let exeeding_red = exeeds max_red "red" game in
        let exeeding_blue = exeeds max_blue "blue" game in
        let exeeding_green = exeeds max_green "green" game in
        (not exeeding_red) && (not exeeding_blue) && not exeeding_green)
      game_list
  in
  let game_ids = List.map (fun game -> game.id) remaining_games in
  list_sum game_ids 0

let update_acc acc draw =
  let as_list = StringMap.to_list draw in
  List.fold_left
    (fun acc curr ->
      let old_red, old_blue, old_green = acc in
      let color, value = curr in
      match color with
      | "red" when value > old_red -> (value, old_blue, old_green)
      | "blue" when value > old_blue -> (old_red, value, old_green)
      | "green" when value > old_green -> (old_red, old_blue, value)
      | _ -> acc)
    acc as_list

let min_number_cubes game =
  List.fold_left (fun acc draw -> update_acc acc draw) (0, 0, 0) game.draws

let part_2 game_list =
  let result = List.map min_number_cubes game_list in
  let multiplied = List.map (fun (a, b, c) -> a * b * c) result in
  list_sum multiplied 0

let _ =
  let input = build_list [] parse_line in
  Printf.printf "[Part1]: %d\n" (part_1 input);
  Printf.printf "[Part2]: %d\n" (part_2 input)
