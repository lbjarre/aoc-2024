open! Core

module Pos = struct
  type t =
    { x : int
    ; y : int
    }
  [@@deriving sexp, compare, equal]

  include (val Comparator.make ~compare ~sexp_of_t)

  let make ~x ~y = { x; y }
  let x t = t.x
  let y t = t.y

  let add_dir { x; y } = function
    | `Up -> { x; y = y - 1 }
    | `Down -> { x; y = y + 1 }
    | `Left -> { x = x - 1; y }
    | `Right -> { x = x + 1; y }
  ;;

  let directions = [ `Up; `Down; `Left; `Right ]
  let neighbors pos = List.map directions ~f:(add_dir pos)
end

let parse input =
  input
  |> String.split_lines
  |> List.map ~f:String.to_list
  |> List.foldi
       ~init:(Map.empty (module Pos))
       ~f:(fun y acc row ->
         List.foldi row ~init:acc ~f:(fun x acc plot ->
           Map.set acc ~key:(Pos.make ~x ~y) ~data:plot))
;;

module Region = struct
  type t =
    { plant : char
    ; plots : Set.M(Pos).t
    }

  let of_grid plots =
    (* DFS search to find all neighboring plots with the same plant value. *)
    let rec dfs_region plant remaining plots stack =
      match stack with
      | [] -> plots, remaining
      | pos :: stack ->
        (match Map.find remaining pos with
         | Some p when Char.equal p plant ->
           let remaining = Map.remove remaining pos in
           let region = Set.add plots pos in
           let stack = List.append (Pos.neighbors pos) stack in
           dfs_region plant remaining region stack
         | _ -> dfs_region plant remaining plots stack)
    in
    (* Pop one plot off the map and DFS-search the region, keep doing it until
       the map is empty. *)
    let rec loop remaining regions =
      match Sequence.hd @@ Map.to_sequence remaining with
      | None -> regions
      | Some (pos, plant) ->
        let plots = Set.singleton (module Pos) pos in
        let stack = Pos.neighbors pos in
        let remaining = Map.remove remaining pos in
        let plots, remaining = dfs_region plant remaining plots stack in
        let regions = { plant; plots } :: regions in
        loop remaining regions
    in
    loop plots []
  ;;

  let mem t = Set.mem t.plots
  let size t = Set.length t.plots

  let perimiter t =
    t.plots
    |> Set.fold ~init:[] ~f:(fun acc pos ->
      Pos.neighbors pos
      |> List.filter ~f:(fun p -> not @@ mem t p)
      |> List.fold ~init:acc ~f:(fun acc v -> v :: acc))
    |> List.length
  ;;

  let sides t =
    Pos.directions
    |> List.map ~f:(fun dir ->
      let major, minor =
        match dir with
        | `Up | `Down -> Pos.y, Pos.x
        | `Left | `Right -> Pos.x, Pos.y
      in
      t.plots
      (* Keep plots at an edge against the current direction. *)
      |> Set.filter ~f:(fun pos -> not @@ mem t (Pos.add_dir pos dir))
      (* Sort and group them on the major axis, results in a list of
         edge-positions per row/col. *)
      |> Set.to_list
      |> List.sort_and_group ~compare:(fun a b -> Int.compare (major a) (major b))
      (* For each row/col, calculate how many contiguous groups we have on the
         minor axis. *)
      |> List.map ~f:(fun axis ->
        axis
        |> List.map ~f:minor
        |> List.sort ~compare:Int.compare
        |> List.group ~break:(fun a b -> b - a <> 1)
        |> List.length)
      |> List.fold ~init:0 ~f:( + ))
    |> List.fold ~init:0 ~f:( + )
  ;;
end

let total_price ~price = List.fold ~init:0 ~f:(fun acc region -> acc + price region)
let price_part1 region = Region.size region * Region.perimiter region
let price_part2 region = Region.size region * Region.sides region
let solve_part1 = total_price ~price:price_part1
let solve_part2 = total_price ~price:price_part2

let solve input =
  let plots = parse input in
  let regions = Region.of_grid plots in
  let part1 = solve_part1 regions in
  printf "part1: %d\n" part1;
  let part2 = solve_part2 regions in
  printf "part2: %d\n" part2
;;

let%expect_test "example input" =
  solve
    {|RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE
|};
  [%expect {|
    part1: 1930
    part2: 1206
    |}]
;;
