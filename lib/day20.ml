open! Core

module Vec = struct
  type t =
    { x : int
    ; y : int
    }
  [@@deriving sexp, compare, equal]

  include (val Comparator.make ~compare ~sexp_of_t)

  let add a b = { x = a.x + b.x; y = a.y + b.y }
  let manhattan a b = abs (a.x - b.x) + abs (a.y - b.y)

  let directions =
    [ { x = 0; y = 1 }; { x = 0; y = -1 }; { x = 1; y = 0 }; { x = -1; y = 0 } ]
  ;;
end

type maze =
  { walls : Set.M(Vec).t
  ; height : int
  ; width : int
  ; start : Vec.t
  ; goal : Vec.t
  }

let of_string str =
  let grid = str |> String.split_lines |> List.map ~f:String.to_list in
  let height = List.length grid in
  let width = List.length (List.hd_exn grid) in
  let walls, start, goal =
    List.foldi
      grid
      ~init:(Set.empty (module Vec), None, None)
      ~f:(fun y (walls, start, goal) row ->
        List.foldi row ~init:(walls, start, goal) ~f:(fun x (walls, start, goal) cell ->
          match cell with
          | '#' -> Set.add walls Vec.{ x; y }, start, goal
          | 'S' -> walls, Some Vec.{ x; y }, goal
          | 'E' -> walls, start, Some Vec.{ x; y }
          | _ -> walls, start, goal))
  in
  let start = Option.value_exn start in
  let goal = Option.value_exn goal in
  { walls; height; width; start; goal }
;;

let dists maze =
  let at_wall = Set.mem maze.walls in
  let not_at_wall vec = not @@ at_wall vec in
  let q = Queue.of_list [ maze.start ] in
  let dist = ref @@ Map.singleton (module Vec) maze.start 0 in
  let rec loop () =
    match Queue.dequeue q with
    | Some v ->
      Vec.directions
      |> List.map ~f:(Vec.add v)
      |> List.filter ~f:not_at_wall
      |> List.filter ~f:(fun v -> not @@ Map.mem !dist v)
      |> List.iter ~f:(fun v' ->
        let vcost = Map.find_exn !dist v in
        dist := Map.set !dist ~key:v' ~data:(vcost + 1);
        Queue.enqueue q v');
      loop ()
    | None -> !dist
  in
  loop ()
;;

let unique_pairs list =
  match list with
  | [] -> Sequence.empty
  | hd :: tl ->
    Sequence.unfold_step ~init:(hd, tl, tl) ~f:(fun (hd, tl, curr) ->
      let open Sequence.Step in
      match curr, tl with
      | [], [] -> Done
      | [], hd :: tl -> Skip { state = hd, tl, tl }
      | p2 :: curr', _ ->
        let value = hd, p2
        and state = hd, tl, curr' in
        Yield { value; state })
;;

let%expect_test "unique_pairs" =
  print_s [%sexp (unique_pairs [ 1; 2; 3; 4 ] |> Sequence.to_list : (int * int) list)];
  [%expect {| ((1 2) (1 3) (1 4) (2 3) (2 4) (3 4)) |}]
;;

let solve_part1 maze =
  let dists = dists maze |> Map.to_alist in
  unique_pairs dists
  |> Sequence.map ~f:(fun ((v1, dist1), (v2, dist2)) ->
    let cheat_dist = Vec.manhattan v1 v2 in
    let orig_dist = abs (dist1 - dist2) in
    let saved = orig_dist - cheat_dist in
    if cheat_dist = 2 && saved >= 100 then 1 else 0)
  |> Sequence.fold ~init:0 ~f:( + )
;;

let solve_part2 maze =
  let dists = dists maze |> Map.to_alist in
  unique_pairs dists
  |> Sequence.map ~f:(fun ((v1, dist1), (v2, dist2)) ->
    let cheat_dist = Vec.manhattan v1 v2 in
    let orig_dist = abs (dist1 - dist2) in
    let saved = orig_dist - cheat_dist in
    if cheat_dist <= 20 && saved >= 100 then 1 else 0)
  |> Sequence.fold ~init:0 ~f:( + )
;;

let solve input =
  let maze = of_string input in
  let part1 = solve_part1 maze in
  printf "part1: %d\n" part1;
  let part2 = solve_part2 maze in
  printf "part2: %d\n" part2
;;
