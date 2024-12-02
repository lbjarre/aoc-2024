open Base

module Instruction = struct
  type t =
    | Mul of int * int
    | Do
    | Dont

  let parse =
    let open Angstrom in
    let number =
      let open Let_syntax in
      let* str = take_while Char.is_digit in
      let len = String.length str in
      if Int.between ~low:1 ~high:3 len
      then return (Int.of_string str)
      else fail "wrong length"
    in
    let mul_instr =
      let open Let_syntax in
      let* _ = string "mul(" in
      let* a = number in
      let* _ = string "," in
      let* b = number in
      let* _ = string ")" in
      return (Mul (a, b))
    in
    let do_instr = string "do()" >>| fun _ -> Do in
    let dont_instr = string "don't()" >>| fun _ -> Dont in
    mul_instr <|> do_instr <|> dont_instr
  ;;
end

let parse =
  let open Angstrom in
  let either_mul_or_none =
    let some_instr = Instruction.parse >>| Option.some in
    let always_takes_one = take 1 >>| fun _ -> Option.None in
    some_instr <|> always_takes_one
  in
  let parser =
    let open Let_syntax in
    let* results = many either_mul_or_none in
    return (List.filter_opt results)
  in
  parse_string ~consume:Consume.All parser
;;

let solve_part1 =
  List.fold ~init:0 ~f:(fun acc instr ->
    match instr with
    | Instruction.Mul (a, b) -> acc + (a * b)
    | _ -> acc)
;;

let solve_part2 instructions =
  instructions
  |> List.fold ~init:(`Enabled, 0) ~f:(fun (state, acc) instr ->
    match state, instr with
    | `Enabled, Instruction.Mul (a, b) -> state, acc + (a * b)
    | _, Instruction.Do -> `Enabled, acc
    | _, Instruction.Dont -> `Disabled, acc
    | _ -> state, acc)
  |> snd
;;

let solve input =
  match parse input with
  | Ok instructions ->
    let part1 = solve_part1 instructions in
    let part2 = solve_part2 instructions in
    Stdio.printf "part1: %d\n" part1;
    Stdio.printf "part2: %d\n" part2
  | Error err -> failwith err
;;
