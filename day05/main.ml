let ic = stdin

let rec grouped_by_emptyline result list =
  match list with
  | [] -> List.rev result
  | head :: tail when String.length head == 0 ->
      grouped_by_emptyline ([] :: result) tail
  | head :: tail -> (
      match result with
      | hd :: tl -> grouped_by_emptyline ((hd @ [ head ]) :: tl) tail
      | _ -> raise (Failure "impossible"))

let rec build_list l =
  match input_line ic with
  | line -> build_list (line :: l)
  | exception End_of_file ->
      close_in ic;
      grouped_by_emptyline [ [] ] (List.rev l)

let parse_int_list str =
  str |> String.trim |> String.split_on_char ' '
  |> List.map (fun n -> int_of_string n)

let parse_block input = input |> List.tl |> List.map parse_int_list

let map_number number map =
  let maybe_result =
    List.find_map
      (fun range ->
        let start_destination = List.nth range 0 in
        let start_source = List.nth range 1 in
        let range_length = List.nth range 2 in
        if number >= start_source && number < start_source + range_length then
          Some (start_destination + (number - start_source))
        else None)
      map
  in
  match maybe_result with Some value -> value | _ -> number

let part_1 input =
  let seeds_raw = List.nth input 0 in
  let seed_to_soil = List.nth input 1 |> parse_block in
  let soil_to_fertilizer = List.nth input 2 |> parse_block in
  let fertilizer_to_water = List.nth input 3 |> parse_block in
  let water_to_light = List.nth input 4 |> parse_block in
  let light_to_temperature = List.nth input 5 |> parse_block in
  let temperature_to_humidity = List.nth input 6 |> parse_block in
  let humidity_to_location = List.nth input 7 |> parse_block in
  let seeds =
    List.nth (String.split_on_char ':' (List.nth seeds_raw 0)) 1
    |> parse_int_list
  in

  let locations =
    seeds
    |> List.map (fun seed ->
           let soil = map_number seed seed_to_soil in
           let fertilizer = map_number soil soil_to_fertilizer in
           let water = map_number fertilizer fertilizer_to_water in
           let light = map_number water water_to_light in
           let temperature = map_number light light_to_temperature in
           let humidity = map_number temperature temperature_to_humidity in
           let location = map_number humidity humidity_to_location in
           location)
  in
  locations
  |> List.fold_left
       (fun acc curr -> if curr < acc then curr else acc)
       (List.hd locations)

let part_2 _ = 0

let _ =
  let input = build_list [] in
  Printf.printf "[Part1]: %d\n" (part_1 input);
  Printf.printf "[Part2]: %d\n" (part_2 input)
