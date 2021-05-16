open Core
open Printf

let ( let+ ) x f = Option.map ~f x

type password_data = {
  first : int;
  second : int;
  letter : char;
  password : string;
}

let parse_dashed dashed =
  match String.split ~on:'-' dashed with
  | [ min; max ] -> Some (Int.of_string min, Int.of_string max)
  | _ -> None

(* Line format is:         *)
(* 13-14 f: ffffffffnfffvv *)
let parse_password_data line =
  match String.split ~on:' ' line with
  | [ dashed; letter_colon; password ] ->
      let letter = letter_colon.[0] in
      let+ first, second = parse_dashed dashed in
      { first; second; letter; password }
  | _ -> None

let rule_1 data : bool =
  let letter_count =
    String.count data.password ~f:(fun lett -> Char.equal data.letter lett)
  in
  data.first <= letter_count && letter_count <= data.second

let rule_2 data : bool =
  let pass = data.password in
  let first_occurs = Char.equal pass.[data.first - 1] data.letter in
  let second_occurs = Char.equal pass.[data.second - 1] data.letter in
  not (Bool.equal first_occurs second_occurs)

let count_valid_passwords ~rule password_list =
  List.map password_list ~f:rule |> List.count ~f:(Bool.equal true)

let () =
  let password_list =
    In_channel.read_lines "./input/02.txt"
    |> List.filter_map ~f:parse_password_data
  in
  let part_1_count = count_valid_passwords ~rule:rule_1 password_list in
  printf "Part 1: %i\n" part_1_count;
  let part_2_count = count_valid_passwords ~rule:rule_2 password_list in
  printf "Part 2: %i\n" part_2_count
