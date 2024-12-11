open Core

let read_input day =
  let fname = "input/day" ^ day ^ ".txt" in
  Stdio.In_channel.read_all fname
;;

module type SOLUTION = sig
  val solve : string -> unit
end

let solution_of_day = function
  | "01" -> (module Aoc.Day01 : SOLUTION)
  | "02" -> (module Aoc.Day02 : SOLUTION)
  | "03" -> (module Aoc.Day03 : SOLUTION)
  | "04" -> (module Aoc.Day04 : SOLUTION)
  | "05" -> (module Aoc.Day05 : SOLUTION)
  | "06" -> (module Aoc.Day06 : SOLUTION)
  | "07" -> (module Aoc.Day07 : SOLUTION)
  | "08" -> (module Aoc.Day08 : SOLUTION)
  | "09" -> (module Aoc.Day09 : SOLUTION)
  | "10" -> (module Aoc.Day10 : SOLUTION)
  | "11" -> (module Aoc.Day11 : SOLUTION)
  | _ -> failwith "day not implemented yet"
;;

let solve day =
  let (module Solution) = solution_of_day day in
  Solution.solve (read_input day)
;;

let cmd =
  Command.basic
    ~summary:"Run AOC 2024 solutions"
    (let%map_open.Command day = anon ("day" %: string) in
     fun () -> solve day)
;;

let () = Command_unix.run cmd
