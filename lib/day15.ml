open! Core

module Vec = struct
  type t =
    { x : int
    ; y : int
    }
  [@@deriving sexp, equal, compare]

  include (val Comparator.make ~compare ~sexp_of_t)

  let add a b = { x = a.x + b.x; y = a.y + b.y }
  let ( + ) = add
  let mul_comp a b = { x = a.x * b.x; y = a.y * b.y }
  let ( *. ) = mul_comp
  let mul sc vec = { x = sc * vec.x; y = sc * vec.y }
  let ( * ) = mul
end

module Problem = struct
  type move =
    | Up
    | Down
    | Left
    | Right
  [@@deriving sexp, equal, compare]

  let move_of_char = function
    | '^' -> Up
    | 'v' -> Down
    | '<' -> Left
    | '>' -> Right
    | _ -> raise @@ Invalid_argument "not a move"
  ;;

  let move_to_vec =
    Vec.(
      function
      | Up -> { x = 0; y = -1 }
      | Down -> { x = 0; y = 1 }
      | Left -> { x = -1; y = 0 }
      | Right -> { x = 1; y = 0 })
  ;;

  module Box = struct
    type width =
      | Single
      | Double
    [@@deriving sexp, equal, compare]

    type t =
      { pos : Vec.t
      ; width : width
      }
    [@@deriving sexp, equal, compare]

    include (val Comparator.make ~compare ~sexp_of_t)

    let push box ~dir = { box with pos = Vec.(box.pos + dir) }

    let is_at box ~vec =
      match box.width with
      | Single -> Vec.equal vec box.pos
      | Double -> Vec.equal vec box.pos || Vec.equal vec Vec.(box.pos + { x = 1; y = 0 })
    ;;
  end

  type t =
    { height : int
    ; width : int
    ; walls : Set.M(Vec).t
    ; boxes : Box.t list
    ; robot : Vec.t
    ; moves : move list
    }
  [@@deriving sexp, equal, compare]

  let is_inbounds t Vec.{ x; y } = x >= 0 && x < t.width && y >= 0 && y < t.height

  let at t vec =
    if is_inbounds t vec
    then
      if Set.mem t.walls vec
      then `Wall
      else (
        match List.find t.boxes ~f:(Box.is_at ~vec) with
        | Some box -> `Box box
        | None -> `Empty)
    else failwith "out of bounds"
  ;;

  let to_string t =
    List.map (List.init t.height ~f:Fn.id) ~f:(fun y ->
      List.map (List.init t.width ~f:Fn.id) ~f:(fun x ->
        let vec = Vec.{ x; y } in
        match at t vec with
        | `Box { width = Single; _ } -> 'O'
        | `Box { width = Double; pos } when Vec.equal pos vec -> '['
        | `Box { width = Double; _ } -> ']'
        | `Wall -> '#'
        | `Empty -> if Vec.equal t.robot vec then '@' else '.')
      |> String.of_list)
    |> String.concat ~sep:"\n"
  ;;

  let of_string str =
    let negate f x = not @@ f x in
    let grid, moves =
      match str |> String.split_lines |> List.split_while ~f:(negate String.is_empty) with
      | hd, "" :: tl -> String.concat ~sep:"\n" hd, String.concat ~sep:"\n" tl
      | _ -> failwith "unreachable"
    in
    let grid = grid |> String.split_lines |> List.map ~f:String.to_list in
    let height = List.length grid
    and width = List.length @@ List.hd_exn grid in
    let walls, boxes, robot =
      List.foldi
        grid
        ~init:(Set.empty (module Vec), [], None)
        ~f:(fun y acc row ->
          List.foldi row ~init:acc ~f:(fun x (walls, boxes, robot) cell ->
            match cell with
            | '#' -> Set.add walls Vec.{ x; y }, boxes, robot
            | 'O' -> walls, Box.{ pos = Vec.{ x; y }; width = Single } :: boxes, robot
            | '@' -> walls, boxes, Some Vec.{ x; y }
            | _ -> walls, boxes, robot))
    in
    let robot = robot |> Option.value_exn in
    let moves =
      moves
      |> String.split_lines
      |> List.bind ~f:String.to_list
      |> List.map ~f:move_of_char
    in
    { height; width; walls; boxes; robot; moves }
  ;;

  let double_width t =
    let width = 2 * t.width in
    let walls =
      Set.fold
        t.walls
        ~init:(Set.empty (module Vec))
        ~f:(fun acc wall ->
          let wall = { wall with x = 2 * wall.x } in
          let acc = Set.add acc wall in
          let acc = Set.add acc { wall with x = wall.x + 1 } in
          acc)
    in
    let boxes =
      List.map t.boxes ~f:(fun box ->
        match box with
        | { pos; width = Single } ->
          Box.{ pos = Vec.(pos *. { x = 2; y = 1 }); width = Double }
        | _ -> failwith "unreachable")
    in
    let robot = Vec.(t.robot *. { x = 2; y = 1 }) in
    { t with width; walls; boxes; robot }
  ;;

  (* Find all pushed boxes when trying to push the box with the given move.
     Returns None if any of the boxes are against a wall, or Some boxes with
     all of the pushed boxes. *)
  let find_pushed_boxes t box move =
    let ( let* ) a f = Option.bind a ~f in
    (* Update the to_check set with whatever is at the position, or signal
       an early return if the position has a wall. *)
    let update_to_check to_check pos =
      match at t pos with
      | `Box box -> Some (Set.add to_check box)
      | `Wall -> None
      | `Empty -> Some to_check
    in
    (* Loop through a to_check set of boxes that we still need to check if they
       can be pushed. *)
    let rec loop to_check pushed =
      match Sequence.hd @@ Set.to_sequence to_check with
      (* No more boxes to check -> done. *)
      | None -> Some pushed
      | Some (box : Box.t) ->
        let to_check = Set.remove to_check box in
        let pushed = box :: pushed in
        let dir = move_to_vec move in
        (match box.width with
         (* A single box only has one position to check. *)
         | Single ->
           let* to_check = update_to_check to_check Vec.(box.pos + dir) in
           loop to_check pushed
         (* A double box needs to check different cells depending on in which
            direction we are pushing it. *)
         | Double ->
           (match move with
            (* Up or down -> two cells to check *)
            | Up | Down ->
              let pos1 = Vec.(box.pos + dir) in
              let pos2 = Vec.(pos1 + { x = 1; y = 0 }) in
              let* to_check = update_to_check to_check pos1 in
              let* to_check = update_to_check to_check pos2 in
              loop to_check pushed
            (* Right -> we only store the boxes leftmost position, so we need
               to check the cell two steps ahead in the push direction. *)
            | Right ->
              let* to_check = update_to_check to_check Vec.(box.pos + (2 * dir)) in
              loop to_check pushed
            (* Left -> since we store the leftmost position it's just the next
               cell in the direction. *)
            | Left ->
              let* to_check = update_to_check to_check Vec.(box.pos + dir) in
              loop to_check pushed))
    in
    loop (Set.singleton (module Box) box) []
  ;;

  let run t =
    let rec loop t = function
      | move :: moves ->
        let dir = move_to_vec move in
        let robot' = Vec.(t.robot + dir) in
        (match at t robot' with
         | `Empty -> loop { t with robot = robot' } moves
         | `Wall -> loop t moves
         | `Box box ->
           (match find_pushed_boxes t box move with
            | None -> loop t moves
            | Some boxes_to_push ->
              let boxes' =
                List.map t.boxes ~f:(fun box ->
                  if List.mem boxes_to_push box ~equal:Box.equal
                  then Box.push box ~dir
                  else box)
              in
              loop { t with robot = robot'; boxes = boxes' } moves))
      | [] -> t
    in
    loop t t.moves
  ;;

  let score_box (box : Box.t) = (100 * box.pos.y) + box.pos.x
  let score t = List.fold t.boxes ~init:0 ~f:(fun acc box -> acc + score_box box)
end

let solve_part1 prob =
  let prob = Problem.run prob in
  Problem.score prob
;;

let solve_part2 prob =
  let prob = Problem.double_width prob in
  let prob = Problem.run prob in
  Problem.score prob
;;

let solve input =
  let prob = Problem.of_string input in
  let part1 = solve_part1 prob in
  printf "part1: %d\n" part1;
  let part2 = solve_part2 prob in
  printf "part2: %d\n" part2
;;

let%expect_test "example input" =
  solve
    {|########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<
|};
  solve
    {|##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^
|};
  [%expect {|
    part1: 2028
    part2: 1751
    part1: 10092
    part2: 9021
    |}]
;;
