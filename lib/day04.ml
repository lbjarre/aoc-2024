open Core

module Grid = struct
  type t = char array array

  let get (t : t) (x, y) =
    try Some t.(y).(x) with
    | Invalid_argument _ -> None
  ;;

  let indicies (t : t) =
    Array.concat_mapi t ~f:(fun y row -> Array.mapi row ~f:(fun x _ -> x, y))
  ;;

  let map (t : t) ~f = t |> indicies |> Array.map ~f

  let of_string str : t =
    str |> String.split_lines |> Array.of_list |> Array.map ~f:String.to_array
  ;;
end

module Direction = struct
  type t =
    | N
    | NE
    | E
    | SE
    | S
    | SW
    | W
    | NW
  [@@deriving sexp]

  let all = [ N; NE; E; SE; S; SW; W; NW ]

  let to_offset = function
    | N -> 0, -1
    | NE -> 1, -1
    | E -> 1, 0
    | SE -> 1, 1
    | S -> 0, 1
    | SW -> -1, 1
    | W -> -1, 0
    | NW -> -1, -1
  ;;

  let add ~dir ~x ~y =
    let dx, dy = to_offset dir in
    x + dx, y + dy
  ;;

  let walk ~dir ~x ~y ~n =
    let dx, dy = to_offset dir in
    List.init n ~f:(fun count -> x + (dx * count), y + (dy * count))
  ;;

  let%expect_test "walk" =
    List.iter all ~f:(fun dir ->
      let walk = walk ~dir ~x:5 ~y:5 ~n:4 in
      print_s [%sexp ((dir, walk) : t * (int * int) list)]);
    [%expect
      {|
      (N ((5 5) (5 4) (5 3) (5 2)))
      (NE ((5 5) (6 4) (7 3) (8 2)))
      (E ((5 5) (6 5) (7 5) (8 5)))
      (SE ((5 5) (6 6) (7 7) (8 8)))
      (S ((5 5) (5 6) (5 7) (5 8)))
      (SW ((5 5) (4 6) (3 7) (2 8)))
      (W ((5 5) (4 5) (3 5) (2 5)))
      (NW ((5 5) (4 4) (3 3) (2 2)))
      |}]
  ;;
end

let count_matches grid ~matches =
  Grid.map grid ~f:(fun (x, y) -> matches grid ~x ~y) |> Array.fold ~init:0 ~f:( + )
;;

let matches_part1 grid ~x ~y =
  let check_direction grid ~dir ~x ~y =
    let walk = Direction.walk ~dir ~x ~y ~n:4 in
    let values = List.map walk ~f:(Grid.get grid) in
    match values with
    | [ Some 'X'; Some 'M'; Some 'A'; Some 'S' ] -> 1
    | _ -> 0
  in
  Direction.all
  |> List.map ~f:(fun dir -> check_direction grid ~dir ~x ~y)
  |> List.fold ~init:0 ~f:( + )
;;

let solve_part1 grid = count_matches grid ~matches:matches_part1

let matches_part2 grid ~x ~y =
  let check_diagonal grid diag =
    let values = List.map diag ~f:(Grid.get grid) in
    match values with
    | [ Some 'M'; Some 'A'; Some 'S' ] | [ Some 'S'; Some 'A'; Some 'M' ] -> `Match
    | _ -> `No_match
  in
  let diagonals =
    Direction.
      [ (let x, y = add ~dir:NE ~x ~y in
         walk ~dir:SW ~x ~y ~n:3)
      ; (let x, y = add ~dir:NW ~x ~y in
         walk ~dir:SE ~x ~y ~n:3)
      ]
  in
  match List.map diagonals ~f:(check_diagonal grid) with
  | [ `Match; `Match ] -> 1
  | _ -> 0
;;

let solve_part2 grid = count_matches grid ~matches:matches_part2

let solve input =
  let grid = Grid.of_string input in
  let part1 = solve_part1 grid in
  let part2 = solve_part2 grid in
  Stdio.printf "part1: %d\n" part1;
  Stdio.printf "part2: %d\n" part2
;;
