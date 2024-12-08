open! Core

let list_pairs lst =
  List.concat_mapi lst ~f:(fun n _ ->
    match List.drop lst n with
    | a :: tl -> List.map tl ~f:(fun b -> a, b)
    | [] -> [])
;;

let%expect_test "list_pairs" =
  let elts = [ 1; 2; 3; 4; 5 ] in
  print_s [%sexp (list_pairs elts : (int * int) list)];
  [%expect {| ((1 2) (1 3) (1 4) (1 5) (2 3) (2 4) (2 5) (3 4) (3 5) (4 5)) |}]
;;

let rec any_true ~f = function
  | hd :: tl -> if f hd then true else any_true tl ~f
  | [] -> false
;;

type antenna =
  { x : int
  ; y : int
  ; freq : char
  }
[@@deriving sexp]

type problem =
  { antennas : antenna list
  ; freqs : char list
  ; width : int
  ; height : int
  }

let parse str =
  let grid = str |> String.split_lines |> List.map ~f:String.to_list in
  let antennas =
    grid
    |> List.mapi ~f:(fun y row ->
      List.mapi row ~f:(fun x ch ->
        match ch with
        | freq when Char.is_alphanum freq -> Some { x; y; freq }
        | _ -> None))
    |> List.concat
    |> List.filter_opt
  in
  let freqs =
    antennas
    |> List.map ~f:(fun antenna -> antenna.freq)
    |> Set.of_list (module Char)
    |> Set.to_list
  in
  let height = List.length grid in
  let width = List.length (List.hd_exn grid) in
  { antennas; freqs; width; height }
;;

let solve_part1 problem =
  let is_double_dist (dx, dy) (dx', dy') = 2 * dx = dx' && 2 * dy = dy' in
  (* Checks if the position x,y has a antinode on the frequency freq. *)
  let pos_has_antinode x y freq =
    problem.antennas
    |> List.filter ~f:(fun antenna -> Char.equal freq antenna.freq)
    |> List.map ~f:(fun antenna -> antenna.x - x, antenna.y - y)
    |> list_pairs
    |> any_true ~f:(fun (a, b) -> is_double_dist a b || is_double_dist b a)
  in
  (* All positions in the grid. *)
  let positions =
    let xs = List.init problem.width ~f:Fn.id in
    let ys = List.init problem.height ~f:Fn.id in
    List.bind xs ~f:(fun x -> List.map ys ~f:(fun y -> x, y))
  in
  positions
  |> List.fold ~init:0 ~f:(fun acc (x, y) ->
    acc + Bool.to_int (any_true problem.freqs ~f:(pos_has_antinode x y)))
;;

module Pos = struct
  module T = struct
    type t = int * int [@@deriving compare, equal, sexp]
  end

  include T
  include Comparator.Make (T)
end

let solve_part2 problem =
  let in_bounds x y = x >= 0 && x < problem.width && y >= 0 && y < problem.height in
  let count_line a b =
    let dx, dy = a.x - b.x, a.y - b.y in
    let antinodes = ref [] in
    let x, y = ref a.x, ref a.y in
    while in_bounds !x !y do
      antinodes := (!x, !y) :: !antinodes;
      x := !x + dx;
      y := !y + dy
    done;
    !antinodes
  in
  let add_to_set lst set = List.fold lst ~init:set ~f:Set.add in
  problem.freqs
  |> List.fold
       ~init:(Set.empty (module Pos))
       ~f:(fun antinodes freq ->
         problem.antennas
         |> List.filter ~f:(fun antenna -> Char.equal freq antenna.freq)
         |> list_pairs
         |> List.fold ~init:antinodes ~f:(fun antinodes (a, b) ->
           antinodes |> add_to_set (count_line a b) |> add_to_set (count_line b a)))
  |> Set.length
;;

let solve input =
  let problem = parse input in
  let part1 = solve_part1 problem in
  let part2 = solve_part2 problem in
  printf "part1: %d\n" part1;
  printf "part2: %d\n" part2
;;

let%expect_test "example input" =
  let example_input =
    {|............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............
|}
  in
  solve example_input;
  [%expect {|
    part1: 14
    part2: 34
    |}]
;;
