open Core
open Printf

let sum_int_list list = List.fold ~init:0 ~f:( + ) list

let increment (n : int option) = match n with Some x -> x + 1 | None -> 1

let count_all_answered answers =
  let count_goal = List.length answers in
  let joined = String.concat answers in
  let count_map =
    String.fold ~init:Char.Map.empty ~f:(Char.Map.update ~f:increment) joined
  in
  Char.Map.count count_map ~f:(fun x -> x = count_goal)

let count_any_answered answers =
  let joined = String.concat answers in
  let answer_set = String.fold ~init:Char.Set.empty ~f:Char.Set.add joined in
  Char.Set.length answer_set

let () =
  let customs_answers =
    In_channel.read_all "./input/06.txt"
    |> Str.split @@ Str.regexp "\n\n"
    |> List.map ~f:(Str.split @@ Str.regexp "\n")
  in

  let any_answered = List.map ~f:count_any_answered customs_answers in
  printf "Part 1: %i\n" @@ sum_int_list any_answered;

  let all_answered = List.map ~f:count_all_answered customs_answers in
  printf "Part 2: %i\n" @@ sum_int_list all_answered
