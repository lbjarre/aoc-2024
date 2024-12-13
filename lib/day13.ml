open! Core

module Pos = struct
  type t =
    { x : int
    ; y : int
    }
end

module Machine = struct
  type t =
    { a : Pos.t
    ; b : Pos.t
    ; prize : Pos.t
    }

  let parser =
    Angstrom.(
      let number = take_while1 Char.is_digit >>| Int.of_string in
      let button_moves =
        let open Let_syntax in
        let* _ = string "X+" in
        let* x = number in
        let* _ = string ", Y+" in
        let* y = number in
        return Pos.{ x; y }
      in
      let button label =
        string "Button " *> string label *> string ": " *> button_moves
      in
      let prize_loc =
        let open Let_syntax in
        let* _ = string "Prize: X=" in
        let* x = number in
        let* _ = string ", Y=" in
        let* y = number in
        return Pos.{ x; y }
      in
      let machine =
        let open Let_syntax in
        let* a = button "A" <* end_of_line in
        let* b = button "B" <* end_of_line in
        let* prize = prize_loc <* end_of_line in
        return { a; b; prize }
      in
      machine)
  ;;

  (* Solve how many A,B button presses are needed to reach the prize using
     Cramer's rule. None means that there are no integer solutions to the
     equation. *)
  let solve t =
    let ( let* ) a f = Option.bind a ~f in
    let guard cond = if cond then Some () else None in
    let det = (t.a.x * t.b.y) - (t.a.y * t.b.x) in
    let* _ = guard (det <> 0) in
    let det_a = (t.prize.x * t.b.y) - (t.prize.y * t.b.x)
    and det_b = (t.a.x * t.prize.y) - (t.a.y * t.prize.x) in
    let a = Float.(of_int det_a / of_int det)
    and b = Float.(of_int det_b / of_int det) in
    let* () = guard Float.(is_integer a && is_integer b) in
    Some Float.(to_int a, to_int b)
  ;;
end

let parse =
  Angstrom.(
    let machines = sep_by1 end_of_line Machine.parser in
    parse_string ~consume:Consume.All machines)
;;

let tokens machines =
  machines
  |> List.map ~f:(fun machine ->
    match Machine.solve machine with
    | None -> 0
    | Some (a, b) -> (3 * a) + b)
  |> List.fold ~init:0 ~f:( + )
;;

let solve_part1 = tokens

let solve_part2 machines =
  machines
  |> List.map ~f:(fun machine ->
    let offset = 10_000_000_000_000 in
    Machine.
      { machine with
        prize = Pos.{ x = offset + machine.prize.x; y = offset + machine.prize.y }
      })
  |> tokens
;;

let solve input =
  match parse input with
  | Ok machines ->
    let part1 = solve_part1 machines in
    printf "part1: %d\n" part1;
    let part2 = solve_part2 machines in
    printf "part2: %d\n" part2
  | Error err -> failwith err
;;

let%expect_test "example input" =
  solve
    {|Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279
|};
  [%expect {|
    part1: 480
    part2: 875318608908
    |}]
;;
