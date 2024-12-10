open! Core

module Pos = struct
  module T = struct
    type t = int * int [@@deriving sexp, compare, equal]
  end

  include T
  include Comparator.Make (T)

  let add (x, y) (x', y') = x + x', y + y'
end

let parse str =
  str
  |> String.split_lines
  |> List.map ~f:String.to_list
  |> List.foldi
       ~init:(Map.empty (module Pos))
       ~f:(fun y map row ->
         List.foldi row ~init:map ~f:(fun x map ch ->
           let height = Int.of_string @@ String.of_char ch in
           Map.set map ~key:(x, y) ~data:height))
;;

module Dir = struct
  type t =
    | Up
    | Down
    | Left
    | Right
  [@@deriving sexp]

  let all = [ Up; Down; Left; Right ]

  let to_delta = function
    | Up -> 0, -1
    | Down -> 0, 1
    | Left -> -1, 0
    | Right -> 1, 0
  ;;
end

let trailhead_score map pos =
  let rec iter pos height reachable =
    Dir.all
    |> List.map ~f:(fun dir ->
      let pos' = Pos.add pos @@ Dir.to_delta dir in
      match height, Map.find map pos' with
      | 8, Some 9 -> Set.singleton (module Pos) pos'
      | n, Some n' when n' = n + 1 -> iter pos' n' reachable
      | _ -> Set.empty (module Pos))
    |> List.fold ~init:reachable ~f:Set.union
  in
  let reachable = iter pos 0 (Set.empty (module Pos)) in
  Set.length reachable
;;

let trailhead_rating map pos =
  let rec iter pos height =
    Dir.all
    |> List.map ~f:(fun dir ->
      let pos' = Pos.add pos @@ Dir.to_delta dir in
      match height, Map.find map pos' with
      | 8, Some 9 -> 1
      | n, Some n' when n' = n + 1 -> iter pos' n'
      | _ -> 0)
    |> List.fold ~init:0 ~f:( + )
  in
  iter pos 0
;;

let count_trailhead_metric map ~f =
  map
  |> Map.filter ~f:(Int.equal 0)
  |> Map.fold ~init:0 ~f:(fun ~key ~data:_ acc -> acc + f map key)
;;

let solve_part1 = count_trailhead_metric ~f:trailhead_score
let solve_part2 = count_trailhead_metric ~f:trailhead_rating

let solve input =
  let map = parse input in
  let part1 = solve_part1 map in
  let part2 = solve_part2 map in
  printf "part1: %d\n" part1;
  printf "part2: %d\n" part2
;;

let%expect_test "example input" =
  solve {|89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732
|};
  [%expect {|
    part1: 36
    part2: 81
    |}]
;;
