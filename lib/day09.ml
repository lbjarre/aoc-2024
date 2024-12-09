open! Core

type slot =
  | File of int
  | Empty

let parse str =
  str
  |> String.chop_suffix_if_exists ~suffix:"\n"
  |> String.to_list
  |> List.mapi ~f:(fun idx ch ->
    let length = Int.of_string (Char.to_string ch) in
    if idx mod 2 = 0
    then (
      let id = idx / 2 in
      File id, length)
    else Empty, length)
;;

let solve_part1 slots =
  let slots =
    slots
    |> List.bind ~f:(fun (slot, length) -> List.init length ~f:(Fn.const slot))
    |> Array.of_list
  in
  let rec incr_until_empty idx =
    match slots.(idx) with
    | Empty -> idx
    | File _ -> incr_until_empty (idx + 1)
  in
  let rec decr_until_file idx =
    match slots.(idx) with
    | File _ -> idx
    | Empty -> decr_until_file (idx - 1)
  in
  let rec iter ~fromi ~toi =
    if fromi < toi
    then ()
    else (
      Array.swap slots fromi toi;
      let fromi = decr_until_file fromi in
      let toi = incr_until_empty toi in
      iter ~fromi ~toi)
  in
  let () =
    let fromi = decr_until_file @@ (Array.length slots - 1) in
    let toi = incr_until_empty 0 in
    iter ~fromi ~toi
  in
  Array.foldi slots ~init:0 ~f:(fun pos acc slot ->
    match slot with
    | File id -> acc + (id * pos)
    | Empty -> acc)
;;

let solve_part2 slots =
  let files =
    slots
    |> List.rev
    |> List.filter_map ~f:(fun (slot, len) ->
      match slot with
      | File id -> Some (id, len)
      | Empty -> None)
  in
  (* Iter through the slots, returning a split at an empty slot which has the
     correct capacity and has not overshot the ID of the file we want to move.
  *)
  let find_empty_slot ~id ~length slots =
    let rec iter hd = function
      (* Empty slot of correct capacity -> return *)
      | (Empty, len) :: tl when length <= len -> Some (List.rev hd, len, tl)
      (* If we find the file ID we are trying to move we have scanned too far,
         files can only move to the left. *)
      | (File i, _) :: _ when i = id -> None
      | slot :: tl -> iter (slot :: hd) tl
      | [] -> None
    in
    iter [] slots
  in
  let compacted =
    List.fold files ~init:slots ~f:(fun slots (id, length) ->
      match find_empty_slot ~id ~length slots with
      (* No valid slot for this file -> just keep going *)
      | None -> slots
      | Some (hd, dst_length, tl) ->
        (* Create the middle section, which is either just the file we are
           moving or the file with a smaller empty space depending on if the
           file occupies the entire space. *)
        let md =
          if dst_length = length
          then [ File id, length ]
          else [ File id, length; Empty, dst_length - length ]
        in
        (* Remove the moved file from the tail. *)
        let tl =
          List.map tl ~f:(fun (l, s) ->
            match l, s with
            | File i, l when l = length && i = id -> Empty, l
            | v -> v)
        in
        List.concat [ hd; md; tl ])
  in
  compacted
  |> List.bind ~f:(fun (slot, length) -> List.init length ~f:(Fn.const slot))
  |> List.foldi ~init:0 ~f:(fun pos acc slot ->
    match slot with
    | File id -> acc + (id * pos)
    | Empty -> acc)
;;

let solve input =
  let slots = parse input in
  let part1 = solve_part1 slots in
  let part2 = solve_part2 slots in
  printf "part1: %d\n" part1;
  printf "part2: %d\n" part2
;;

let%expect_test "example input" =
  let example_input = "2333133121414131402\n" in
  solve example_input;
  [%expect {|
    part1: 1928
    part2: 2858
    |}]
;;
