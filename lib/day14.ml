open! Core

module Vec = struct
  type t =
    { x : int
    ; y : int
    }
  [@@deriving sexp]

  let ( + ) a b = { x = a.x + b.x; y = a.y + b.y }
end

module Robot = struct
  type t =
    { pos : Vec.t
    ; vel : Vec.t
    }
  [@@deriving sexp]

  let parser =
    Angstrom.(
      let open Let_syntax in
      let number =
        let* sign = option Fn.id (string "-" >>| Fn.const Int.neg) in
        let* digits = take_while1 Char.is_digit >>| Int.of_string in
        return @@ sign digits
      in
      let vec label =
        let* _ = string label *> string "=" in
        let* x = number in
        let* _ = string "," in
        let* y = number in
        return Vec.{ x; y }
      in
      let* pos = vec "p" in
      let* _ = string " " in
      let* vel = vec "v" in
      return { pos; vel })
  ;;

  let update t ~bounds =
    let open Vec in
    let pos = t.pos + t.vel in
    let pos = { x = pos.x % bounds.x; y = pos.y % bounds.y } in
    { t with pos }
  ;;
end

let print_tiles robots ~bounds =
  let open Vec in
  let open Robot in
  List.init bounds.y ~f:Fn.id
  |> List.iter ~f:(fun y ->
    let row =
      List.init bounds.x ~f:Fn.id
      |> List.map ~f:(fun x ->
        let count =
          List.count robots ~f:(fun robot -> robot.pos.x = x && robot.pos.y = y)
        in
        if count = 0 then "." else Int.to_string count)
      |> String.concat
    in
    printf "%s\n" row)
;;

let parse =
  Angstrom.(
    let parser = many1 (Robot.parser <* end_of_line) in
    parse_string ~consume:Consume.All parser)
;;

let count_quadrants robots ~bounds =
  let mid_x = Vec.(bounds.x) / 2
  and mid_y = Vec.(bounds.y) / 2 in
  let in_lower_y = Int.between ~low:0 ~high:(mid_y - 1)
  and in_upper_y = Int.between ~low:(mid_y + 1) ~high:bounds.y
  and in_lower_x = Int.between ~low:0 ~high:(mid_x - 1)
  and in_upper_x = Int.between ~low:(mid_x + 1) ~high:bounds.x in
  let in_q1 Vec.{ x; y } = in_lower_x x && in_lower_y y
  and in_q2 Vec.{ x; y } = in_lower_x x && in_upper_y y
  and in_q3 Vec.{ x; y } = in_upper_x x && in_upper_y y
  and in_q4 Vec.{ x; y } = in_upper_x x && in_lower_y y in
  let q1 = List.count robots ~f:(fun Robot.{ pos; _ } -> in_q1 pos)
  and q2 = List.count robots ~f:(fun Robot.{ pos; _ } -> in_q2 pos)
  and q3 = List.count robots ~f:(fun Robot.{ pos; _ } -> in_q3 pos)
  and q4 = List.count robots ~f:(fun Robot.{ pos; _ } -> in_q4 pos) in
  q1, q2, q3, q4
;;

let solve_part1 robots ~bounds =
  List.init 100 ~f:Fn.id
  |> List.fold ~init:robots ~f:(fun robots _ -> List.map robots ~f:(Robot.update ~bounds))
  |> count_quadrants ~bounds
  |> fun (q1, q2, q3, q4) -> q1 * q2 * q3 * q4
;;

let stddev xs =
  let count = List.length xs in
  let sum = List.fold ~init:0 ~f:( + ) xs in
  let mean = Float.(of_int sum / of_int count) in
  let deviations = List.map xs ~f:(fun x -> Float.(Float.int_pow (of_int x - mean) 2)) in
  let dev_sum = List.fold ~init:0.0 ~f:Float.( + ) deviations in
  Float.(dev_sum / of_int count)
;;

let solve_part2 robots ~bounds =
  (* Keep on updating all the robots and stop when we have a clumping of them
     in both x and y axes, as determined by the stddev of the robot positions.
     The threshold of 700 was found by looking at the stddev of some of the
     random frames, which all were above 800. *)
  let rec loop robots n =
    let robots' = List.map robots ~f:(Robot.update ~bounds) in
    let n' = n + 1 in
    let stddev_x = List.map robots' ~f:(fun r -> Robot.(r.pos.x)) |> stddev in
    let stddev_y = List.map robots' ~f:(fun r -> Robot.(r.pos.y)) |> stddev in
    if Float.(stddev_x < 700. && stddev_y < 700.) then n' else loop robots' n'
  in
  loop robots 0
;;

let solve_with_bounds ~bounds input =
  match parse input with
  | Ok robots ->
    let part1 = solve_part1 robots ~bounds in
    printf "part1: %d\n" part1;
    let part2 = solve_part2 robots ~bounds in
    printf "part2 %d\n" part2
  | Error err -> failwith err
;;

let solve = solve_with_bounds ~bounds:Vec.{ x = 101; y = 103 }

let%expect_test "example input" =
  let input =
    {|p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3
|}
  in
  let robots = parse input |> Result.ok_or_failwith in
  let part1 = solve_part1 ~bounds:Vec.{ x = 11; y = 7 } robots in
  printf "part1: %d\n" part1;
  [%expect {| part1: 12 |}]
;;
