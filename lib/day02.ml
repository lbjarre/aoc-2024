open Core

let parse =
  let open Angstrom in
  let parser =
    let number = take_while1 Char.is_digit |> map ~f:Int.of_string in
    let space = string " " in
    let line = sep_by1 space number <* end_of_line in
    many1 line
  in
  parse_string ~consume:Consume.All parser
;;

let rec pairwise = function
  | a :: b :: tl -> (a, b) :: pairwise (b :: tl)
  | _ -> []
;;

let%expect_test "pairwise works" =
  let pairs = pairwise [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ] in
  print_s [%sexp (pairs : (int * int) list)];
  [%expect {| ((1 2) (2 3) (3 4) (4 5) (5 6) (6 7) (7 8) (8 9) (9 10)) |}]
;;

let rec all ~f = function
  | hd :: tl -> if f hd then all tl ~f else false
  | [] -> true
;;

let rec any ~f = function
  | hd :: tl -> if f hd then true else any tl ~f
  | [] -> false
;;

let is_safe report =
  let pairs = pairwise report in
  let diffs = List.map pairs ~f:(fun (a, b) -> a - b) in
  let all_increasing = all diffs ~f:Int.is_positive in
  let all_decreasing = all diffs ~f:Int.is_negative in
  let diff_size = all diffs ~f:(fun d -> Int.abs d > 0 && Int.abs d < 4) in
  (all_increasing || all_decreasing) && diff_size
;;

let is_safe_with_tolerance report =
  let remove_nth_elt n =
    let head = List.take report (n - 1) in
    let tail = List.drop report n in
    List.concat [ head; tail ]
  in
  let all_remove_permutations =
    let len = List.length report in
    List.init len ~f:(fun idx -> remove_nth_elt (idx + 1))
  in
  is_safe report || any (List.map all_remove_permutations ~f:is_safe) ~f:(fun v -> v)
;;

let solve_part1 reports = reports |> List.map ~f:is_safe |> List.count ~f:(fun v -> v)

let solve_part2 reports =
  reports |> List.map ~f:is_safe_with_tolerance |> List.count ~f:(fun v -> v)
;;

let solve input =
  match parse input with
  | Ok reports ->
    let part1 = solve_part1 reports in
    let part2 = solve_part2 reports in
    Stdio.printf "part1: %d\n" part1;
    Stdio.printf "part2: %d\n" part2
  | Error err -> failwith err
;;
