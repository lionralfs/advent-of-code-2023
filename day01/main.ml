open Utils
module CharSet = Set.Make (Char)

let ic = stdin
let digits = CharSet.of_list [ '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' ]

let rec first_num_part_1 chars =
  match chars with
  | [] -> raise (Failure "unable to find number")
  | x :: _ when CharSet.mem x digits -> int_of_string (String.make 1 x)
  | _ :: rest -> first_num_part_1 rest

let rec last_num_part_1 chars =
  match chars with
  | [] -> raise (Failure "unable to find number")
  | x :: _ when CharSet.mem x digits -> int_of_string (String.make 1 x)
  | _ :: rest -> last_num_part_1 rest

let rec first_num_part_2 chars =
  match chars with
  | [] -> raise (Failure "unable to find number")
  | x :: _ when CharSet.mem x digits -> int_of_string (String.make 1 x)
  | 'o' :: 'n' :: 'e' :: _ -> 1
  | 't' :: 'w' :: 'o' :: _ -> 2
  | 't' :: 'h' :: 'r' :: 'e' :: 'e' :: _ -> 3
  | 'f' :: 'o' :: 'u' :: 'r' :: _ -> 4
  | 'f' :: 'i' :: 'v' :: 'e' :: _ -> 5
  | 's' :: 'i' :: 'x' :: _ -> 6
  | 's' :: 'e' :: 'v' :: 'e' :: 'n' :: _ -> 7
  | 'e' :: 'i' :: 'g' :: 'h' :: 't' :: _ -> 8
  | 'n' :: 'i' :: 'n' :: 'e' :: _ -> 9
  | _ :: rest -> first_num_part_2 rest

let rec last_num_part_2 chars =
  match chars with
  | [] -> raise (Failure "unable to find number")
  | x :: _ when CharSet.mem x digits -> int_of_string (String.make 1 x)
  | 'e' :: 'n' :: 'o' :: _ -> 1
  | 'o' :: 'w' :: 't' :: _ -> 2
  | 'e' :: 'e' :: 'r' :: 'h' :: 't' :: _ -> 3
  | 'r' :: 'u' :: 'o' :: 'f' :: _ -> 4
  | 'e' :: 'v' :: 'i' :: 'f' :: _ -> 5
  | 'x' :: 'i' :: 's' :: _ -> 6
  | 'n' :: 'e' :: 'v' :: 'e' :: 's' :: _ -> 7
  | 't' :: 'h' :: 'g' :: 'i' :: 'e' :: _ -> 8
  | 'e' :: 'n' :: 'i' :: 'n' :: _ -> 9
  | _ :: rest -> last_num_part_2 rest

let parse_line fn_first fn_last line =
  let nums = explode line in
  let first = fn_first nums in
  let last = fn_last (List.rev nums) in
  (first * 10) + last

let rec build_list l =
  match input_line ic with
  | line -> build_list (line :: l)
  | exception End_of_file ->
      close_in ic;
      List.rev l

let _ =
  let input = build_list [] in
  let input_1 = List.map (parse_line first_num_part_1 last_num_part_1) input in
  let input_2 = List.map (parse_line first_num_part_2 last_num_part_2) input in
  Printf.printf "[Part1]: %d\n" (list_sum input_1 0);
  Printf.printf "[Part2]: %d\n" (list_sum input_2 0)
