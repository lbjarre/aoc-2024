open Core

module Rule = struct
  type t =
    { before : int
    ; after : int
    }

  let before { before; _ } = before
  let after { after; _ } = after

  let parser =
    Angstrom.(
      let page = take_while1 Char.is_digit >>| Int.of_string in
      let open Let_syntax in
      let* before = page in
      let* _ = string "|" in
      let* after = page in
      return { before; after })
  ;;

  let compare_on_before a b = Int.compare a.before b.before

  (** Takes a list of rules and returns a function returning the count of the
      input value in the before position of the rules. *)
  let before_count rules =
    let counts =
      rules
      |> List.sort ~compare:compare_on_before
      |> List.group ~break:(fun a b -> before a <> before b)
      |> List.map ~f:(fun group ->
        let value = List.hd_exn group |> before in
        value, List.length group)
    in
    fun value ->
      counts
      |> List.find ~f:(fun (v, _) -> equal v value)
      |> Option.value ~default:(value, 0)
      |> snd
  ;;
end

module Update = struct
  type t = int list

  let parser =
    Angstrom.(
      let page = take_while1 Char.is_digit >>| Int.of_string in
      sep_by1 (string ",") page)
  ;;

  let adheres_to_rule (t : t) Rule.{ before; after } =
    let first = List.findi t ~f:(fun _ v -> Int.equal before v) in
    let second = List.findi t ~f:(fun _ v -> Int.equal after v) in
    match first, second with
    | Some (idx_first, _), Some (idx_second, _) -> idx_first < idx_second
    | _ -> true
  ;;

  let is_valid t ~rules =
    List.find rules ~f:(fun v -> not @@ adheres_to_rule t v) |> Option.is_none
  ;;

  (** The middle value in the update list. Raises if the update list is not
      correctly created *)
  let middle t =
    let len = List.length t in
    List.drop t (len / 2) |> List.hd_exn
  ;;
end

module Manual = struct
  type t =
    { rules : Rule.t list
    ; updates : Update.t list
    }

  let parser =
    Angstrom.(
      let open Let_syntax in
      let* rules = many1 (Rule.parser <* end_of_line) in
      let* _ = end_of_line in
      let* updates = many1 (Update.parser <* end_of_line) in
      return { rules; updates })
  ;;

  let parse = Angstrom.(parse_string ~consume:Consume.All parser)

  let solve_part1 { rules; updates } =
    updates
    |> List.filter ~f:(Update.is_valid ~rules)
    |> List.map ~f:Update.middle
    |> List.fold ~init:0 ~f:( + )
  ;;

  (** Filters the rules to the set that concerns the given update. *)
  let relevant_rules update rules =
    let mem v = List.mem ~equal update v in
    let both_mem Rule.{ before; after } = mem before && mem after in
    List.filter rules ~f:both_mem
  ;;

  (** Order the Update according to the rules. *)
  let order update ~rules =
    (* Filter down to only the rules that applies to this update. *)
    let relevant_rules = relevant_rules update rules in
    (* Get a count for how many times each value occurrs as a before-condition
       in the rules, and then sort on this. *)
    let counts = Rule.before_count relevant_rules in
    let compare a b = Int.compare (counts a) (counts b) in
    List.sort update ~compare
  ;;

  let solve_part2 { rules; updates } =
    updates
    |> List.filter ~f:(fun update -> not @@ Update.is_valid update ~rules)
    |> List.map ~f:(order ~rules)
    |> List.map ~f:Update.middle
    |> List.fold ~init:0 ~f:( + )
  ;;
end

let solve input =
  match Manual.parse input with
  | Ok manual ->
    let part1 = Manual.solve_part1 manual in
    let part2 = Manual.solve_part2 manual in
    printf "part1: %d\n" part1;
    printf "part2: %d\n" part2
  | Error err -> failwith err
;;

let%expect_test "examples" =
  let example_input =
    {|47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47
|}
  in
  solve example_input;
  [%expect {|
      part1: 143
      part2: 123
      |}]
;;
