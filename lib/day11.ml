open! Core

let even_digits n =
  let length = String.length @@ Int.to_string n in
  length mod 2 = 0
;;

let split n =
  let digits = String.to_list @@ Int.to_string n in
  let halfway = List.length digits / 2 in
  let hd = Int.of_string @@ String.of_list @@ List.take digits halfway in
  let tl = Int.of_string @@ String.of_list @@ List.drop digits halfway in
  hd, tl
;;

module Key = struct
  module T = struct
    type t =
      { stone : int
      ; blinks : int
      }
    [@@deriving sexp, compare, equal]
  end

  include T
  include Comparator.Make (T)
end

let blink_stone = function
  | 0 -> `Single 1
  | n when even_digits n -> `Split (split n)
  | n -> `Single (n * 2024)
;;

let rec length_n_blinks ~cache stone blinks =
  let compute stone blinks =
    match blinks with
    | 0 -> 1, cache
    | n ->
      (match blink_stone stone with
       | `Single stone' ->
         let length, cache = length_n_blinks ~cache stone' (n - 1) in
         length, cache
       | `Split (a, b) ->
         let length_a, cache = length_n_blinks ~cache a (n - 1) in
         let length_b, cache = length_n_blinks ~cache b (n - 1) in
         length_a + length_b, cache)
  in
  let key = Key.{ stone; blinks } in
  match Map.find cache key with
  | Some length -> length, cache
  | None ->
    let length, cache = compute stone blinks in
    let cache = Map.set cache ~key ~data:length in
    length, cache
;;

let parse input =
  input
  |> String.chop_suffix_if_exists ~suffix:"\n"
  |> String.split ~on:' '
  |> List.map ~f:Int.of_string
;;

let solve_n_blinks stones n =
  List.fold
    stones
    ~init:(Map.empty (module Key), 0)
    ~f:(fun (cache, acc) stone ->
      let length, cache = length_n_blinks ~cache stone n in
      cache, acc + length)
  |> snd
;;

let solve_part1 stones = solve_n_blinks stones 25
let solve_part2 stones = solve_n_blinks stones 75

let solve input =
  let stones = parse input in
  let part1 = solve_part1 stones in
  printf "part1: %d\n" part1;
  let part2 = solve_part2 stones in
  printf "part2: %d\n" part2
;;

let%expect_test "example input" =
  solve "125 17";
  [%expect {|
    part1: 55312
    part2: 65601038650482
    |}]
;;
