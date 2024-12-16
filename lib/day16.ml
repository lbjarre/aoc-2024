open! Core

module Vec = struct
  type t =
    { x : int
    ; y : int
    }
  [@@deriving sexp, equal, compare]

  include (val Comparator.make ~compare ~sexp_of_t)

  let add a b = { x = a.x + b.x; y = a.y + b.y }
end

type maze =
  { walls : Set.M(Vec).t
  ; start : Vec.t
  ; goal : Vec.t
  }

let parse str =
  let grid = str |> String.split_lines |> List.map ~f:String.to_list in
  let walls, start, goal =
    List.foldi
      grid
      ~init:(Set.empty (module Vec), None, None)
      ~f:(fun y acc row ->
        List.foldi row ~init:acc ~f:(fun x (walls, start, goal) cell ->
          match cell with
          | '#' -> Set.add walls Vec.{ x; y }, start, goal
          | 'S' -> walls, Some Vec.{ x; y }, goal
          | 'E' -> walls, start, Some Vec.{ x; y }
          | _ -> walls, start, goal))
  in
  match start, goal with
  | Some start, Some goal -> { walls; start; goal }
  | _ -> failwith "missing start and goal"
;;

type dir =
  | N
  | S
  | E
  | W
[@@deriving sexp, compare, equal]

let dir_to_vec =
  Vec.(
    function
    | N -> { x = 0; y = -1 }
    | S -> { x = 0; y = 1 }
    | E -> { x = -1; y = 0 }
    | W -> { x = 1; y = 0 })
;;

let dir_clockwise = function
  | N -> E
  | S -> W
  | E -> S
  | W -> N
;;

let dir_counterclockwise = function
  | N -> W
  | S -> E
  | E -> N
  | W -> S
;;

module Key = struct
  type t = Vec.t * dir [@@deriving sexp, compare, equal]

  include (val Comparator.make ~compare ~sexp_of_t)
end

let dijkstra maze =
  (* Get all the next possible states from the (vec,dir) state and their
     associated immediate cost. *)
  let next_states vec dir =
    let check dir turn_cost =
      let vec' = Vec.add vec (dir_to_vec dir) in
      if Set.mem maze.walls vec' then None else Some (vec', dir, turn_cost + 1)
    in
    [ check dir 0; check (dir_clockwise dir) 1000; check (dir_counterclockwise dir) 1000 ]
    |> List.filter_opt
  in
  (* Init dijkstra state, which consists of
     - A prio queue for the next states to evaluate
     - A map (state -> cost) for tracking the min cost to reach any state
     - A map (state -> [state, cost]) for tracking the previous states in the
       best paths for any given state. *)
  let queue =
    Pairing_heap.of_list
      [ maze.start, E, 0 ]
      ~cmp:(fun (_, _, a_cost) (_, _, b_cost) -> Int.compare a_cost b_cost)
  in
  let dists = Map.singleton (module Key) (maze.start, E) 0 in
  let prevs = Map.empty (module Key) in
  (* Main eval loop. *)
  let rec loop dists prevs =
    match Pairing_heap.pop queue with
    (* No more items in the queue -> done evaluating. *)
    | None -> dists, prevs
    | Some (vec, dir, _) ->
      (* Get the current known best cost to reach this state. *)
      let cost = Map.find dists (vec, dir) |> Option.value_exn in
      (* Get the next possible states from this one, and add the cost to get
         here to the immediate cost to reach that next state. *)
      let states' =
        next_states vec dir
        |> List.map ~f:(fun (vec, dir, immediate_cost) -> vec, dir, immediate_cost + cost)
      in
      (* Update queue,dists,prevs with the new states.
         Note that the queue is mutable. *)
      let dists, prevs =
        List.fold
          states'
          ~init:(dists, prevs)
          ~f:(fun (dists, prevs) (vec', dir', cost) ->
            (* Get the currently known best cost to reach this state, default
               to INT_MAX. *)
            let curr_cost =
              Map.find dists (vec', dir') |> Option.value ~default:Int.max_value
            in
            (* Is the new cost better or equal than the current one?
               Note that its >= and not > since we want to find _all_ the best
               paths for part2. *)
            if curr_cost >= cost (* If so, update the dijkstra states. *)
            then (
              Pairing_heap.add queue (vec', dir', cost);
              let dists = Map.set dists ~key:(vec', dir') ~data:cost in
              (* Updating the paths is a bit more tricky since we might need
                 to evict previous entries when we had an earlier path that was
                 not as good as this one. This is why we also keep track of
                 the cost in the paths so we can filter them here. *)
              let prevs =
                Map.update prevs (vec', dir') ~f:(fun curr_paths_opt ->
                  match curr_paths_opt with
                  | None -> [ vec, dir, cost ]
                  | Some curr_paths ->
                    (* Remove any paths that are now no longer optimal. *)
                    let curr_paths =
                      List.filter curr_paths ~f:(fun (_, _, curr_cost) ->
                        curr_cost = cost)
                    in
                    (* Only add this one if it's already not in there. *)
                    let rec loop = function
                      | [] -> (vec, dir, cost) :: curr_paths
                      | (vec', dir', _) :: tl ->
                        if Key.equal (vec', dir') (vec, dir) then curr_paths else loop tl
                    in
                    loop curr_paths)
              in
              dists, prevs)
            else dists, prevs)
      in
      loop dists prevs
  in
  loop dists prevs
;;

let solve_part1 maze =
  let dists, _ = dijkstra maze in
  let _goal_state, cost =
    Map.to_alist dists
    |> List.filter ~f:(fun ((vec, _dir), _cost) -> Vec.equal vec maze.goal)
    |> List.hd_exn (* There is only one way of get into the goal state afaict *)
  in
  cost
;;

let solve_part2 maze =
  let dists, paths = dijkstra maze in
  let goal_state, _cost =
    Map.to_alist dists
    |> List.filter ~f:(fun ((vec, _dir), _cost) -> Vec.equal vec maze.goal)
    |> List.hd_exn (* There is only one way of get into the goal state afaict *)
  in
  (* Loop through all the optimal paths and add the tiles to a set. *)
  let rec loop tiles state =
    let tiles = Set.add tiles (fst state) in
    if Key.equal state (maze.start, E)
    then tiles
    else (
      match Map.find paths state with
      | Some prev_states ->
        List.fold prev_states ~init:tiles ~f:(fun tiles (vec, dir, _) ->
          loop tiles (vec, dir))
      | None -> failwith "unreachable")
  in
  let tiles = Set.empty (module Vec) in
  let tiles = loop tiles goal_state in
  Set.length tiles
;;

let solve input =
  let maze = parse input in
  let part1 = solve_part1 maze in
  printf "part1: %d\n" part1;
  let part2 = solve_part2 maze in
  printf "part2: %d\n" part2
;;

let%expect_test "example input" =
  solve
    {|###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############
|};
  solve
    {|#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################
    |};
  [%expect {|
    part1: 7036
    part2: 45
    part1: 11048
    part2: 64
    |}]
;;
