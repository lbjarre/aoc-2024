open Base

let parse_lists input =
  let open Angstrom in
  let parser =
    let number = take_while1 Char.is_digit |> map ~f:Int.of_string in
    let line =
      let open Let_syntax in
      let* first = number in
      let* _ = string "   " in
      let* second = number in
      return (first, second)
    in
    let transpose =
      let f (list_a, list_b) (a, b) = a :: list_a, b :: list_b in
      List.fold ~init:([], []) ~f
    in
    let parser = sep_by1 end_of_line line <* end_of_line |> map ~f:transpose in
    parser
  in
  parse_string ~consume:Consume.All parser input
;;

let solve_part1 (a, b) =
  let a = List.sort a ~compare:Int.compare in
  let b = List.sort b ~compare:Int.compare in
  List.zip_exn a b
  |> List.map ~f:(fun (a, b) -> Int.abs (a - b))
  |> List.fold ~init:0 ~f:( + )
;;

let solve_part2 (left, right) =
  left
  |> List.map ~f:(fun v ->
    let score = List.count right ~f:(Int.equal v) in
    v * score)
  |> List.fold ~init:0 ~f:( + )
;;

let solve input =
  match parse_lists input with
  | Ok lists ->
    let part1 = solve_part1 lists in
    let part2 = solve_part2 lists in
    Stdio.printf "part1: %d\n" part1;
    Stdio.printf "part2: %d\n" part2
  | Error err -> failwith err
;;
