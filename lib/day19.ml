open! Core

module Trie = struct
  type t = Node of string option * t Map.M(Char).t [@@deriving sexp]

  let empty = Node (None, Map.empty (module Char))

  let find t key =
    let rec _find t key =
      match key, t with
      | [], Node (None, _) -> None
      | [], Node (Some v, _) -> Some v
      | hd :: tl, Node (_, m) ->
        Option.bind (Map.find m hd) ~f:(fun subtrie -> _find subtrie tl)
    in
    _find t (String.to_list key)
  ;;

  let add t str =
    let rec _add t key =
      match key, t with
      | [], Node (_, m) -> Node (Some str, m)
      | ch :: tl, Node (v, m) ->
        let subtrie = Map.find m ch |> Option.value ~default:empty in
        let subtrie = _add subtrie tl in
        Node (v, Map.set m ~key:ch ~data:subtrie)
    in
    _add t (String.to_list str)
  ;;

  let of_list = List.fold ~init:empty ~f:add
end

let parse input =
  match String.split_lines input with
  | towels :: "" :: designs ->
    let towels =
      towels |> String.substr_replace_all ~pattern:", " ~with_:"," |> String.split ~on:','
    in
    towels, designs
  | _ -> failwith "bad input"
;;

let memo = ref (Map.empty (module String))

(* Memoized trie search. *)
let rec search_trie trie pattern =
  match Map.find !memo pattern with
  | Some r -> r
  | None ->
    let res =
      (* Iter through all possible prefix lengths of the pattern. *)
      List.init (String.length pattern) ~f:(( + ) 1)
      |> List.map ~f:(fun len ->
        let prefix = String.prefix pattern len in
        let suffix = String.drop_prefix pattern len in
        match Trie.find trie prefix with
        (* Prefix exists in the trie and is the whole pattern:
           one match. *)
        | Some _s when String.equal suffix "" -> 1
        (* Prefix exists in the trie:
           search how many ways we can match the suffix *)
        | Some _s -> search_trie trie suffix
        (* Prefix does not exist:
           no match *)
        | None -> 0)
      |> List.fold ~init:0 ~f:( + )
    in
    memo := Map.set !memo ~key:pattern ~data:res;
    res
;;

let solve input =
  let towels, designs = parse input in
  let trie = Trie.of_list towels in
  let possible_arrangements = List.map designs ~f:(search_trie trie) in
  let part1 = List.count possible_arrangements ~f:(fun count -> count <> 0) in
  printf "part1: %d\n" part1;
  let part2 = List.fold possible_arrangements ~init:0 ~f:( + ) in
  printf "part2: %d\n" part2
;;
