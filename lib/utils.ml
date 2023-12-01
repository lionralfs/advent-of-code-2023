let rec range a b = if a > b then [] else a :: range (a + 1) b

let flatten xs =
  let rec aux xs acc =
    match xs with
    | [] -> acc
    | [] :: tail -> aux tail acc
    | hd :: tail -> aux tail acc @ hd
  in
  aux xs []

let rec list_sum list acc =
  match list with [] -> acc | head :: tail -> list_sum tail acc + head

let explode s = List.init (String.length s) (String.get s)