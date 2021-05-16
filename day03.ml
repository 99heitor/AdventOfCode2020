open Core
open Printf

let generate_tree_map (input : string list) : int -> int -> bool option =
 fun x ->
  match List.nth input x with
  | Some row ->
      fun y -> Some (Char.equal '#' (String.get row (y mod String.length row)))
  | _ -> fun _ -> None

let generate_slide_stream tree_map x y =
  Stream.from (fun i -> tree_map (i * x) (i * y))

let count_trees_in_path tree_map x y =
  let slide_stream = generate_slide_stream tree_map x y in
  let tree_count = ref 0 in
  Stream.iter
    (fun (tree : bool) -> if tree then tree_count := !tree_count + 1)
    slide_stream;
  !tree_count

let () =
  let tree_counter =
    In_channel.read_lines "./input/03.txt"
    |> generate_tree_map |> count_trees_in_path
  in
  let part_1_count = tree_counter 1 3 in
  printf "Part 1: %i\n" part_1_count;
  let part_2_count =
    [
      tree_counter 1 1;
      tree_counter 1 3;
      tree_counter 1 5;
      tree_counter 1 7;
      tree_counter 2 1;
    ]
  in
  printf "Part 2: %i\n" (List.fold part_2_count ~init:1 ~f:(fun x y -> x * y))
