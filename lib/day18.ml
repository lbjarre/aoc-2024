open! Core

module Vec = struct
  type t =
    { x : int
    ; y : int
    }
  [@@deriving sexp, compare, equal]

  include (val Comparator.make ~compare ~sexp_of_t)

  let add a b = { x = a.x + b.x; y = a.y + b.y }
end

let parser =
  Angstrom.(
    let open Let_syntax in
    let number = take_while1 Char.is_digit >>| Int.of_string in
    let vec =
      let* x = number in
      let* _ = string "," in
      let* y = number in
      return Vec.{ x; y }
    in
    sep_by1 end_of_line vec <* end_of_line)
;;

let parse = Angstrom.(parse_string ~consume:Consume.All parser)

type t =
  { bytes : Vec.t list
  ; bounds : Vec.t * Vec.t
  }

let is_in_bounds t vec =
  Vec.(
    let { x = min_x; y = min_y } = fst t.bounds in
    let { x = max_x; y = max_y } = snd t.bounds in
    Int.between ~low:min_x ~high:max_x vec.x && Int.between ~low:min_y ~high:max_y vec.y)
;;

let dijkstra ~n t =
  let ( let* ) a f = Option.bind a ~f in
  let corrupted =
    List.take t.bytes n |> List.fold ~init:(Set.empty (module Vec)) ~f:Set.add
  in
  let neighbors vec =
    Vec.[ { x = 0; y = 1 }; { x = 0; y = -1 }; { x = 1; y = 0 }; { x = -1; y = 0 } ]
    |> List.map ~f:(Vec.add vec)
    |> List.filter ~f:(is_in_bounds t)
    |> List.filter ~f:(fun vec -> not @@ Set.mem corrupted vec)
  in
  let start = fst t.bounds in
  let goal = snd t.bounds in
  let queue =
    Pairing_heap.of_list [ start, 0 ] ~cmp:(fun (_, a) (_, b) -> Int.compare a b)
  in
  let dists = Map.singleton (module Vec) start 0 in
  let rec loop dists =
    let* vec, _ = Pairing_heap.pop queue in
    let* cost = Map.find dists vec in
    if Vec.equal vec goal
    then Some cost
    else (
      let next_vecs = neighbors vec in
      let dists =
        List.fold next_vecs ~init:dists ~f:(fun dists vec' ->
          let curr_cost = Map.find dists vec' |> Option.value ~default:Int.max_value in
          if curr_cost > cost + 1
          then (
            (* update *)
            Pairing_heap.add queue (vec', cost + 1);
            Map.set dists ~key:vec' ~data:(cost + 1))
          else dists)
      in
      loop dists)
  in
  loop dists
;;

let solve_part1 t =
  dijkstra t ~n:1024 |> Option.value_exn ~message:"maze should be solvable"
;;

let solve_part2 t =
  let rec loop n =
    match dijkstra t ~n with
    | Some _cost -> loop (n + 1)
    | None -> List.take t.bytes n |> List.last_exn
  in
  loop 1024
;;

let solve input =
  let bounds = Vec.({ x = 0; y = 0 }, { x = 70; y = 70 }) in
  let bytes = parse input |> Result.ok_or_failwith in
  let t = { bytes; bounds } in
  let part1 = solve_part1 t in
  printf "part1: %d\n" part1;
  let part2 = solve_part2 t in
  printf "part2: %d,%d\n" part2.x part2.y
;;
