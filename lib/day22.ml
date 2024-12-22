open! Core

let evolve secret =
  let mix a b = Int.bit_xor a b in
  let prune a = a % 16777216 in
  let secret = 64 * secret |> mix secret |> prune in
  let secret = secret / 32 |> mix secret |> prune in
  let secret = 2048 * secret |> mix secret |> prune in
  secret
;;

let%expect_test "evolve" =
  List.init 10 ~f:Fn.id
  |> List.folding_map ~init:123 ~f:(fun secret _ ->
    let secret' = evolve secret in
    secret', secret')
  |> List.iter ~f:(printf "%d\n");
  [%expect
    {|
    15887950
    16495136
    527345
    704524
    1553684
    12683156
    11100544
    12249484
    7753432
    5908254
    |}]
;;

let solve_part1 secrets =
  let evolve_n n secret =
    let rec loop s = function
      | 0 -> s
      | n -> loop (evolve s) (n - 1)
    in
    loop secret n
  in
  secrets |> List.map ~f:(evolve_n 2000) |> List.fold ~init:0 ~f:( + )
;;

let%expect_test "part1 example" =
  printf "part1: %d\n" @@ solve_part1 [ 1; 10; 100; 2024 ];
  [%expect {| part1: 37327623 |}]
;;

module Delta_seq = struct
  type t = int list [@@deriving sexp, compare]

  include (val Comparator.make ~compare ~sexp_of_t)
end

let deltaseq_of_secret secret =
  let price secret = secret % 10 in
  let add_delta deltas delta =
    match deltas with
    | [] -> [ delta ]
    | [ d0 ] -> [ d0; delta ]
    | [ d0; d1 ] -> [ d0; d1; delta ]
    | [ d0; d1; d2 ] -> [ d0; d1; d2; delta ]
    | [ _d0; d1; d2; d3 ] -> [ d1; d2; d3; delta ]
    | _ -> failwith "unreachable"
  in
  let lup = Map.empty (module Delta_seq) in
  let deltas = [] in
  let lup, _, _ =
    List.init 2000 ~f:Fn.id
    |> List.fold ~init:(lup, secret, deltas) ~f:(fun (lup, secret, deltas) _ ->
      let p0 = price secret in
      let secret' = evolve secret in
      let p1 = price secret' in
      let delta = p0 - p1 in
      let deltas' = add_delta deltas delta in
      match List.length deltas' with
      | 4 ->
        (match Map.add lup ~key:deltas' ~data:p1 with
         | `Ok lup -> lup, secret', deltas'
         | `Duplicate -> lup, secret', deltas')
      | _ -> lup, secret', deltas')
  in
  lup
;;

let solve_part2 secrets =
  let lups = List.map secrets ~f:deltaseq_of_secret in
  let total = Map.empty (module Delta_seq) in
  let total =
    List.fold lups ~init:total ~f:(fun total lup ->
      Map.fold lup ~init:total ~f:(fun ~key ~data total ->
        match Map.find total key with
        | Some v -> Map.set total ~key ~data:(data + v)
        | None -> Map.set total ~key ~data))
  in
  Map.data total |> List.max_elt ~compare:Int.compare |> Option.value_exn
;;

let%expect_test "part2 example" =
  printf "part2: %d\n" @@ solve_part2 [ 1; 2; 3; 2024 ];
  [%expect {| part2: 23 |}]
;;

let parse input = input |> String.split_lines |> List.map ~f:Int.of_string

let solve input =
  let secrets = parse input in
  let part1 = solve_part1 secrets in
  printf "part1: %d\n" part1;
  let part2 = solve_part2 secrets in
  printf "part2: %d\n" part2
;;
