open Core
open Option
open Printf

let ( let+ ) x f = Option.map ~f x

type comparison = Lt | Eq | Gt

let comp x y = if x < y then Lt else if x > y then Gt else Eq

let rec find2sum_rec sorted reversed sum : (int * int) option =
  match (sorted, reversed) with
  | hs :: ts, hr :: tr -> (
      match comp (hs + hr) sum with
      | Eq -> Some (hs, hr)
      | Gt -> find2sum_rec sorted tr sum
      | Lt -> find2sum_rec ts reversed sum)
  | _ -> None

let find2sum input sum =
  let sorted = List.sort ~compare:(fun a b -> a - b) input in
  let reversed = List.rev sorted in
  find2sum_rec sorted reversed sum

let find3sum input sum =
  List.find_map input ~f:(fun c ->
      let+ a, b = find2sum input (sum - c) in
      (a, b, c))

let part1 input sum = find2sum input sum |> map ~f:(fun (a, b) -> a * b)

let part2 input sum = find3sum input sum |> map ~f:(fun (a, b, c) -> a * b * c)

let () =
  let input =
    In_channel.read_lines "./input/01.txt" |> List.map ~f:Int.of_string
  in
  (match part1 input 2020 with
  | Some x -> printf "Part 1 answer: %i\n" x
  | None -> printf "Part 1 answer not found.\n");
  match part2 input 2020 with
  | Some x -> printf "Part 2 answer: %i\n" x
  | None -> printf "Part 2 answer not found.\n"
