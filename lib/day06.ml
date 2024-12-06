open Core

module Grid = struct
  module Coord = struct
    module T = struct
      type t = int * int [@@deriving equal, compare, sexp]

      let add (x, y) (x', y') = x + x', y + y'
    end

    include T
    include Comparator.Make (T)
  end

  type 'a t =
    { cells : 'a array
    ; height : int
    ; width : int
    }
  [@@deriving sexp]

  let coord_to_offset { width; _ } (x, y) = x + (width * y)

  let offset_to_coord { width; _ } offset =
    let x = offset mod width in
    let y = offset / width in
    x, y
  ;;

  let coord_in_range { width; height; _ } (x, y) =
    Int.between x ~low:0 ~high:(width - 1) && Int.between y ~low:0 ~high:(height - 1)
  ;;

  let cell t coord =
    if coord_in_range t coord
    then (
      let offset = coord_to_offset t coord in
      Some t.cells.(offset))
    else None
  ;;

  let of_string str =
    let rows = String.split_lines str in
    let height = List.length rows in
    let col = List.hd_exn rows in
    let width = String.length col in
    let cells = rows |> List.map ~f:String.to_array |> Array.concat in
    { cells; width; height }
  ;;

  let map t ~f =
    let cells = Array.map t.cells ~f in
    { t with cells }
  ;;

  let mapi t ~f =
    let cells =
      Array.mapi t.cells ~f:(fun offset cell -> f (offset_to_coord t offset) cell)
    in
    { t with cells }
  ;;

  let foldi t ~init ~f =
    Array.foldi t.cells ~init ~f:(fun offset acc cell ->
      f (offset_to_coord t offset) acc cell)
  ;;

  let to_string t ~f =
    t.cells
    |> Array.mapi ~f:(fun offset cell -> f (offset_to_coord t offset) cell)
    |> List.of_array
    |> List.chunks_of ~length:t.width
    |> List.map ~f:String.of_list
    |> String.concat ~sep:"\n"
  ;;
end

module Problem = struct
  type direction =
    | Up
    | Down
    | Left
    | Right
  [@@deriving sexp, compare]

  let turn_right = function
    | Up -> Right
    | Right -> Down
    | Down -> Left
    | Left -> Up
  ;;

  let direction_to_dxdy = function
    | Up -> 0, -1
    | Right -> 1, 0
    | Down -> 0, 1
    | Left -> -1, 0
  ;;

  let add dir (x, y) =
    let dx, dy = direction_to_dxdy dir in
    x + dx, y + dy
  ;;

  module Guard = struct
    module T = struct
      type t =
        { coord : Grid.Coord.t
        ; direction : direction
        }
      [@@deriving sexp, compare]
    end

    include T
    include Comparator.Make (T)

    let to_char guard =
      match guard.direction with
      | Up -> '^'
      | Down -> 'v'
      | Left -> '<'
      | Right -> '>'
    ;;
  end

  type cell =
    | Empty
    | Occupied
  [@@deriving sexp, equal]

  let cell_of_char = function
    | '.' | '^' -> Empty
    | '#' -> Occupied
    | _ -> failwith "unknown cell"
  ;;

  type t =
    { grid : cell Grid.t
    ; guard : Guard.t
    }
  [@@deriving sexp]

  let of_string str =
    let grid = Grid.of_string str in
    let offset, _ = Array.findi_exn grid.cells ~f:(fun _ cell -> Char.equal cell '^') in
    let () = Array.set grid.cells offset '.' in
    let grid = Grid.map grid ~f:cell_of_char in
    let start = Grid.offset_to_coord grid offset in
    let guard = Guard.{ coord = start; direction = Up } in
    { grid; guard }
  ;;

  let trace_path t =
    let rec iter t ~coords ~guard_states =
      let next_guard_coord = add t.guard.direction t.guard.coord in
      match Grid.cell t.grid next_guard_coord with
      | Some Empty ->
        let coords = Set.add coords next_guard_coord in
        let guard = { t.guard with coord = next_guard_coord } in
        iter { t with guard } ~coords ~guard_states
      | Some Occupied when Set.mem guard_states t.guard -> `Looped
      | Some Occupied ->
        let guard_states = Set.add guard_states t.guard in
        let direction = turn_right t.guard.direction in
        let guard = { t.guard with direction } in
        iter { t with guard } ~coords ~guard_states
      | None -> `Exited coords
    in
    let coords = Set.singleton (module Grid.Coord) t.guard.coord in
    let guard_states = Set.empty (module Guard) in
    iter t ~coords ~guard_states
  ;;

  let solve_part1 t =
    match trace_path t with
    | `Exited visited -> Set.length visited
    | `Looped -> failwith "part1 should exit"
  ;;

  let solve_part2 t =
    Grid.foldi t.grid ~init:0 ~f:(fun coord acc cell ->
      match cell with
      (* Skip the guard starting position. *)
      | _ when Grid.Coord.equal coord t.guard.coord -> acc
      (* Skip cells that are already occupied. *)
      | Occupied -> acc
      (* For all other empty cells, try to replace it and see if we loop. *)
      | Empty ->
        (* Create a new grid with the empty cell replaced with an occupied one. *)
        let grid' =
          Grid.mapi t.grid ~f:(fun c cell ->
            if Grid.Coord.equal c coord then Occupied else cell)
        in
        (match trace_path { t with grid = grid' } with
         | `Exited _ -> acc
         | `Looped -> acc + 1))
  ;;
end

let solve input =
  let problem = Problem.of_string input in
  let part1 = Problem.solve_part1 problem in
  let part2 = Problem.solve_part2 problem in
  printf "part1: %d\n" part1;
  printf "part2: %d\n" part2
;;

let%expect_test "examples" =
  let example_input =
    {|....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...
|}
  in
  solve example_input;
  [%expect {|
    part1: 41
    part2: 6
    |}]
;;
