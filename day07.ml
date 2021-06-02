open Core
open Printf

(* Input parsing *)

type bag_count = { bag : string; count : int }

type rule = { subject : string; might_contain : bag_count list }

let parse_bag_count bag_count_as_list =
  List.filter_map bag_count_as_list ~f:(fun x ->
      let split = Str.split (Str.regexp " ") x in
      match split with
      | [ count; modifier; color ] ->
          Some
            {
              count = Int.of_string count;
              bag = String.concat ~sep:" " [ modifier; color ];
            }
      | [ _no; _other ] -> None
      | _ -> raise @@ Failure (sprintf "Bag count has invalid format: %s" x))

let parse_rule rule_as_list =
  match rule_as_list with
  | hd :: tl -> { subject = hd; might_contain = parse_bag_count tl }
  | [] -> raise @@ Failure "Rule has invalid format"

(* Graph building for part 1*)

let update_parent_edges parent_list parent count =
  match parent_list with
  | Some parent_list -> { bag = parent; count } :: parent_list
  | None -> [ { bag = parent; count } ]

let update_parent_graph_with_rule graph rule =
  let { subject; might_contain } = rule in
  List.fold might_contain ~init:graph ~f:(fun graph { bag; count } ->
      Map.update graph bag ~f:(fun old_parent_list ->
          update_parent_edges old_parent_list subject count))

let generate_parent_graph rules =
  List.fold rules
    ~init:(Map.empty (module String))
    ~f:update_parent_graph_with_rule

(* Graph building for part 2*)

let generate_child_graph rules =
  let open String in
  List.fold rules ~init:Map.empty ~f:(fun graph { subject; might_contain } ->
      Map.set graph ~key:subject ~data:might_contain)

(* Part 1 *)

let rec outermost_parent_set_rec bag_color rule_graph set =
  match Map.find rule_graph bag_color with
  | Some parent_list ->
      let open String in
      Set.union_list
      @@ set
         ::
         List.map parent_list ~f:(fun { bag = parent_bag; _ } ->
             outermost_parent_set_rec parent_bag rule_graph
               (Set.singleton parent_bag))
  | None -> set

let outermost_parent_set bag_color rule_graph =
  outermost_parent_set_rec bag_color rule_graph (Set.empty (module String))

let part_1 rules =
  let parent_graph = generate_parent_graph rules in
  Set.length @@ outermost_parent_set "shiny gold" parent_graph

(* Part 2*)

let rec count_containing_bags bag_color rule_graph =
  match Map.find rule_graph bag_color with
  | Some child_list ->
      List.sum (module Int) ~f:ident
      @@ List.map child_list ~f:(fun { bag = child_bag; count = child_count } ->
             child_count
             + (child_count * count_containing_bags child_bag rule_graph))
  | None -> 0

let part_2 rules =
  let child_graph = generate_child_graph rules in
  count_containing_bags "shiny gold" child_graph

let () =
  let rules =
    In_channel.read_lines "./input/07.txt"
    |> List.map ~f:(fun line ->
           Str.split (Str.regexp {|\( bags contain \| bags?. ?\)|}) line)
    |> List.map ~f:parse_rule
  in
  printf "Part 1: %i\n" @@ part_1 rules;
  printf "Part 2: %i\n" @@ part_2 rules
