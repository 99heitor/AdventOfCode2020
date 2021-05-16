open Core
open Printf
open Option.Monad_infix

type passport = {
  byr : int option;
  iyr : int option;
  eyr : int option;
  hgt : string option;
  hcl : string option;
  ecl : string option;
  pid : string option;
  cid : string option;
}

type length = In | Cm

let length_of_string s =
  let open String in
  if s = "in" then Some In else if s = "cm" then Some Cm else None

type height = { measure : int; measure_unit : length }

let ( let+ ) x f = Option.map ~f x

let parse_height height =
  let open Str in
  try
    ignore (search_forward (regexp {|\([0-9]+\)\(in\|cm\)|}) height 0);
    let measure_string, unit_string =
      (matched_group 1 height, matched_group 2 height)
    in
    let+ measure_unit = length_of_string unit_string in
    { measure = Int.of_string measure_string; measure_unit }
  with Caml.Not_found -> None

let find_parameter raw_passport name =
  let open Str in
  try
    ignore
      (search_forward (regexp (name ^ {|:\([#a-zA-Z0-9]+\)|})) raw_passport 0);
    Some (matched_group 1 raw_passport)
  with Caml.Not_found -> None

let regex_first_group rgx str =
  let open Str in
  try
    ignore (search_forward (regexp rgx) str 0);
    Some (matched_group 1 str)
  with Caml.Not_found -> None

let some_and_true b_opt = match b_opt with Some x -> x && true | None -> false

let first_group_matches_length rgx str lght =
  some_and_true
    (let+ matched = regex_first_group rgx str in
     String.length matched = lght)

let parse_port raw =
  let find_in_passport = find_parameter raw in
  {
    byr = find_in_passport "byr" >>| Int.of_string;
    iyr = find_in_passport "iyr" >>| Int.of_string;
    eyr = find_in_passport "eyr" >>| Int.of_string;
    hgt = find_in_passport "hgt";
    hcl = find_in_passport "hcl";
    ecl = find_in_passport "ecl";
    pid = find_in_passport "pid";
    cid = find_in_passport "cid";
  }

let check_range min max n = min <= n && n <= max

let check_eye color =
  List.mem
    [ "amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth" ]
    color ~equal:String.equal

let check_hair code = first_group_matches_length {|^#\([0-9a-f]+\)$|} code 6

let check_pid id = first_group_matches_length {|^\([0-9]+\)$|} id 9

let check_height { measure; measure_unit } =
  match measure_unit with
  | Cm -> check_range 150 193 measure
  | In -> check_range 59 76 measure

let is_valid_1 p =
  is_some p.byr && is_some p.iyr && is_some p.eyr && is_some p.hgt
  && is_some p.hcl && is_some p.ecl && is_some p.pid

let is_valid_2 p =
  List.for_all ~f:some_and_true
    [
      p.byr >>| check_range 1920 2002;
      p.iyr >>| check_range 2010 2020;
      p.eyr >>| check_range 2020 2030;
      p.hgt >>= parse_height >>| check_height;
      p.hcl >>| check_hair;
      p.ecl >>| check_eye;
      p.pid >>| check_pid;
    ]

let () =
  let passports =
    In_channel.read_all "./input/04.txt"
    |> Str.split (Str.regexp "\n\n")
    |> List.map ~f:parse_port
  in
  List.count passports ~f:is_valid_1 |> printf "Part 1: %i\n";
  List.count passports ~f:is_valid_2 |> printf "Part 2: %i\n"
