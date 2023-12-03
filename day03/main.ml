open Utils
module CharSet = Set.Make (Char)

let ic = stdin

type node_type = Number | Symbol

type node = {
  node_type : node_type;
  start_x : int;
  end_x : int;
  y_index : int;
  value : string;
}

let _string_of_node node =
  let node_name =
    match node.node_type with Number -> "Number" | Symbol -> "Symbol"
  in
  "(" ^ string_of_int node.start_x ^ "-" ^ string_of_int node.end_x ^ ","
  ^ string_of_int node.y_index ^ " " ^ node_name ^ " " ^ node.value ^ ")"

let digits =
  CharSet.of_list [ '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' ]

let is_digit x = CharSet.mem x digits

let rec build_list l y_index line_parsing_fn =
  match input_line ic with
  | line ->
      build_list
        (line_parsing_fn y_index line :: l)
        (y_index + 1) line_parsing_fn
  | exception End_of_file ->
      close_in ic;
      List.rev l

let rec parse_line_aux y_index x_index chars result =
  match chars with
  | '.' :: rest -> parse_line_aux y_index (x_index + 1) rest result
  | x :: y :: z :: rest when is_digit x && is_digit y && is_digit z ->
      let node =
        {
          node_type = Number;
          start_x = x_index;
          end_x = x_index + 2;
          y_index;
          value = cl2s [ x; y; z ];
        }
      in
      parse_line_aux y_index (x_index + 3) rest (node :: result)
  | x :: y :: rest when is_digit x && is_digit y ->
      let node =
        {
          node_type = Number;
          start_x = x_index;
          end_x = x_index + 1;
          y_index;
          value = cl2s [ x; y ];
        }
      in
      parse_line_aux y_index (x_index + 2) rest (node :: result)
  | x :: rest when is_digit x ->
      let node =
        {
          node_type = Number;
          start_x = x_index;
          end_x = x_index;
          y_index;
          value = cl2s [ x ];
        }
      in
      parse_line_aux y_index (x_index + 1) rest (node :: result)
  | [] -> List.rev result
  | x :: rest ->
      let node =
        {
          node_type = Symbol;
          start_x = x_index;
          end_x = x_index;
          y_index;
          value = String.make 1 x;
        }
      in
      parse_line_aux y_index (x_index + 1) rest (node :: result)

let parse_line y_index line =
  let chars = explode line in
  parse_line_aux y_index 0 chars []

let partition nodes =
  List.partition (fun node -> node.node_type == Symbol) nodes

let is_adjacent number_node symbol_node =
  let y_diff = abs (number_node.y_index - symbol_node.y_index) in
  let x_diff_left = abs (number_node.start_x - symbol_node.start_x) in
  let x_diff_right = abs (number_node.end_x - symbol_node.start_x) in
  y_diff <= 1 && (x_diff_left <= 1 || x_diff_right <= 1)

let number_node_value number_node = int_of_string number_node.value

let part_1 symbols numbers =
  let adjacent_to_symbol =
    List.filter
      (fun number_node ->
        List.exists
          (fun symbol_node -> is_adjacent number_node symbol_node)
          symbols)
      numbers
  in
  let as_numbers = List.map number_node_value adjacent_to_symbol in
  list_sum as_numbers 0

let part_2 symbols numbers =
  let potential_gears =
    List.filter (fun symbol_node -> symbol_node.value = "*") symbols
  in
  List.fold_left
    (fun acc current_symbol ->
      let adjacent_numbers =
        List.filter
          (fun number_node -> is_adjacent number_node current_symbol)
          numbers
      in
      match adjacent_numbers with
      | [ a; b ] -> acc + (number_node_value a * number_node_value b)
      | _ -> acc)
    0 potential_gears

let _ =
  let input = build_list [] 0 parse_line in
  let symbols, numbers = partition (flatten input) in
  Printf.printf "[Part1]: %d\n" (part_1 symbols numbers);
  Printf.printf "[Part2]: %d\n" (part_2 symbols numbers)
