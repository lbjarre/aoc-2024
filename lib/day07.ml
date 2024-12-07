open! Core

let rec permutations elts = function
  | 1 -> List.map elts ~f:List.singleton
  | n ->
    let ( let* ) a f = List.bind a ~f in
    let ( let+ ) a f = List.map a ~f in
    let tails = permutations elts (n - 1) in
    let* elt = elts in
    let+ tail = tails in
    elt :: tail
;;

let%expect_test "permutations" =
  let elts = [ 1; 2 ] in
  let perms = permutations elts 3 in
  print_s [%sexp (perms : int list list)];
  [%expect {| ((1 1 1) (1 1 2) (1 2 1) (1 2 2) (2 1 1) (2 1 2) (2 2 1) (2 2 2)) |}]
;;

let rec any_true ~f = function
  | hd :: tl -> if f hd then true else any_true ~f tl
  | [] -> false
;;

module Equation = struct
  type t =
    { test_value : int
    ; numbers : int list
    }

  let parser =
    Angstrom.(
      let number = take_while1 Char.is_digit >>| Int.of_string in
      let open Let_syntax in
      let* test_value = number in
      let* _ = string ": " in
      let* numbers = sep_by1 (string " ") number in
      return { test_value; numbers })
  ;;

  type operator =
    | Add
    | Multiply
    | Concat

  let apply = function
    | Add -> ( + )
    | Multiply -> fun a b -> a * b
    | Concat ->
      fun a b ->
        let a = Int.to_string a in
        let b = Int.to_string b in
        Int.of_string (a ^ b)
  ;;

  let could_be_true { numbers; test_value } ~operations =
    let init, numbers =
      match numbers with
      | hd :: tl -> hd, tl
      | [] -> failwith "must have at least one number"
    in
    let perms = permutations operations (List.length numbers) in
    any_true perms ~f:(fun ops ->
      List.zip_exn ops numbers
      |> List.fold ~init ~f:(fun lnum (op, rnum) -> apply op lnum rnum)
      |> Int.equal test_value)
  ;;
end

let parse =
  Angstrom.(
    let parser = many1 (Equation.parser <* end_of_line) in
    parse_string parser ~consume:Consume.All)
;;

let solve_with_operations equations ~operations =
  equations
  |> List.filter ~f:(Equation.could_be_true ~operations)
  |> List.fold ~init:0 ~f:(fun acc Equation.{ test_value; _ } -> acc + test_value)
;;

let solve_part1 = solve_with_operations ~operations:[ Add; Multiply ]
let solve_part2 = solve_with_operations ~operations:[ Add; Multiply; Concat ]

let solve input =
  match parse input with
  | Ok equations ->
    let part1 = solve_part1 equations in
    let part2 = solve_part2 equations in
    printf "part1: %d\n" part1;
    printf "part2: %d\n" part2
  | Error err -> failwith err
;;

let%expect_test "example input" =
  let example_input =
    {|190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20
|}
  in
  solve example_input;
  [%expect {|
    part1: 3749
    part2: 11387
    |}]
;;
