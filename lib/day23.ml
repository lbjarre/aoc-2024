open! Core

let parse input =
  input
  |> String.split_lines
  |> List.map ~f:(fun line ->
    match String.split line ~on:'-' with
    | [ a; b ] -> a, b
    | _ -> failwith "")
;;

let adjacency_list conns =
  let map = Map.empty (module String) in
  let add map a b = Map.add_multi map ~key:a ~data:b in
  List.fold conns ~init:map ~f:(fun map (a, b) ->
    let map = add map a b in
    let map = add map b a in
    map)
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

module Group = struct
  type t = string list [@@deriving compare, sexp]

  include (val Comparator.make ~compare ~sexp_of_t)
end

let solve_part1 conns =
  let adj = adjacency_list conns in
  let set = Set.empty (module Group) in
  Map.to_alist adj
  |> List.fold ~init:set ~f:(fun set (a, conns) ->
    unique_pairs conns
    |> Sequence.filter ~f:(fun (b, c) ->
      List.mem (Map.find_exn adj b) c ~equal:String.equal)
    |> Sequence.map ~f:(fun (b, c) -> List.sort [ a; b; c ] ~compare:String.compare)
    |> Sequence.fold ~init:set ~f:Set.add)
  |> Set.filter ~f:(fun group ->
    List.find group ~f:(String.is_prefix ~prefix:"t") |> Option.is_some)
  |> Set.length
;;

let solve_part2 conns =
  let adj = adjacency_list conns |> Map.map ~f:(Set.of_list (module String)) in
  let neighbors node =
    Map.find adj node |> Option.value ~default:(Set.empty (module String))
  in
  (* Bron-Kerbosch algorithm for finding maximal cliques in undirected graphs.
     https://en.wikipedia.org/wiki/Bron%E2%80%93Kerbosch_algorithm *)
  let rec bron_kerbosch clique possible excluded =
    if Set.is_empty possible && Set.is_empty excluded
    then Sequence.singleton clique
    else (
      let possible = ref possible
      and excluded = ref excluded in
      Set.to_sequence !possible
      |> Sequence.bind ~f:(fun v ->
        possible := Set.remove !possible v;
        let clique' = Set.add clique v in
        let possible' = Set.inter !possible (neighbors v) in
        let excluded' = Set.inter !excluded (neighbors v) in
        let subseq = bron_kerbosch clique' possible' excluded' in
        excluded := Set.add !excluded v;
        subseq))
  in
  let nodes = Map.keys adj in
  let r = Set.empty (module String)
  and p = Set.of_list (module String) nodes
  and x = Set.empty (module String) in
  bron_kerbosch r p x
  |> Sequence.max_elt ~compare:(fun a b -> Int.compare (Set.length a) (Set.length b))
  |> Option.value_exn
  |> Set.to_list
  |> List.sort ~compare:String.compare
  |> String.concat ~sep:","
;;

let solve input =
  let conns = parse input in
  let part1 = solve_part1 conns in
  printf "part1: %d\n" part1;
  let part2 = solve_part2 conns in
  printf "part2: %s\n" part2
;;
