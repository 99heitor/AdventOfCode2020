open Core

let get_binary c =
  let open Char in
  if c = 'F' || c = 'L' then 0 else 1

let sum_binary_place i acc value =
  let open Int in
  let binary = get_binary value in
  acc + (binary * (2 ** i))

let chair_coord directions = String.foldi directions ~f:sum_binary_place ~init:0

let seat_id address =
  let open String in
  let reversed = String.rev address in
  let row = chair_coord @@ suffix reversed 7 in
  let column = chair_coord @@ prefix reversed 3 in
  (row * 8) + column

let rec find_discontinuity l =
  match l with
  | hd1 :: hd2 :: tl ->
      if hd2 - hd1 <> 1 then Some (hd1 + 1) else find_discontinuity (hd2 :: tl)
  | _ -> None

let part_1 ids = List.max_elt ids ~compare:Int.compare

let part_2 ids = List.sort ids ~compare:Int.compare |> find_discontinuity

let () =
  let ids = In_channel.read_lines "./input/05.txt" |> List.map ~f:seat_id in
  Printf.printf "Part 1: %i\n" (match part_1 ids with Some x -> x | _ -> 0);
  Printf.printf "Part 2: %i\n" (match part_2 ids with Some x -> x | _ -> 0)
