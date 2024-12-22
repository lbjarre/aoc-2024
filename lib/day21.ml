open! Core

module Vec = struct
  type t =
    { x : int
    ; y : int
    }
  [@@deriving sexp, compare, equal]

  include (val Comparator.make ~compare ~sexp_of_t)

  let add a b = { x = a.x + b.x; y = a.y + b.y }
  let init x y = { x; y }

  let of_dir = function
    | '<' -> init (-1) 0
    | '>' -> init 1 0
    | '^' -> init 0 (-1)
    | 'v' -> init 0 1
    | _ -> failwith "not a dir"
  ;;
end

type keypad = Vec.t Map.M(Char).t [@@deriving sexp, compare]

let numpad =
  (* +---+---+---+
     | 7 | 8 | 9 |
     +---+---+---+
     | 4 | 5 | 6 |
     +---+---+---+
     | 1 | 2 | 3 |
     +---+---+---+
     ....| 0 | A |
     ....+---+---+ *)
  Map.of_alist_exn
    (module Char)
    [ '0', Vec.init 1 3
    ; 'A', Vec.init 2 3
    ; '1', Vec.init 0 2
    ; '2', Vec.init 1 2
    ; '3', Vec.init 2 2
    ; '4', Vec.init 0 1
    ; '5', Vec.init 1 1
    ; '6', Vec.init 2 1
    ; '7', Vec.init 0 0
    ; '8', Vec.init 1 0
    ; '9', Vec.init 2 0
    ]
;;

let dirpad =
  (* ....+---+---+
     ....| ^ | A |
     +---+---+---+
     | < | v | > |
     +---+---+---+ *)
  Map.of_alist_exn
    (module Char)
    [ '<', Vec.init 0 1
    ; 'v', Vec.init 1 1
    ; '>', Vec.init 2 1
    ; '^', Vec.init 1 0
    ; 'A', Vec.init 2 0
    ]
;;

let rec permutations list =
  let ins_all_positions x l =
    let rec aux prev acc = function
      | [] -> (prev @ [ x ]) :: acc |> List.rev
      | hd :: tl as l -> aux (prev @ [ hd ]) ((prev @ [ x ] @ l) :: acc) tl
    in
    aux [] [] l
  in
  match list with
  | [] -> []
  | x :: [] -> [ [ x ] ]
  | x :: xs ->
    List.fold ~init:[] ~f:(fun acc p -> acc @ ins_all_positions x p) (permutations xs)
;;

let%expect_test "permutations" =
  let str = "012" in
  let perms = permutations (String.to_list str) |> List.map ~f:String.of_list in
  print_s [%sexp (perms : string list)];
  [%expect {| (012 102 120 021 201 210) |}]
;;

let dirpad_start = Map.find_exn dirpad 'A'

module Memo_key = struct
  type t = char list * keypad list * Vec.t [@@deriving sexp, compare]

  include (val Comparator.make ~compare ~sexp_of_t)
end

let memo = ref @@ Map.empty (module Memo_key)

(* Memoize the results of cost. *)
let memo_cost seq stack start ~f =
  match Map.find !memo (seq, stack, start) with
  | Some v -> v
  | None ->
    let v = f seq stack start in
    memo := Map.set !memo ~key:(seq, stack, start) ~data:v;
    v
;;

(** cost seq stack start finds the optimal cost of entering the sequence seq
    on the given stack of keypads given that we start of the key on the given
    starting vector. *)
let rec cost (seq : char list) (stack : keypad list) (start : Vec.t) =
  match seq, stack with
  (* Sequence is empty -> no cost. *)
  | [], _ -> 0
  (* No more keypads in the stack -> the cost is just the set of instructions. *)
  | _, [] -> List.length seq
  (* We want to navigate to the button `hd` on the current pad `pad`, but also
     have the remaining stack `stack'` and the remaining sequence `tl`. *)
  | hd :: tl, pad :: stack' ->
    let target = Map.find_exn pad hd in
    (* Find the total number of button presses needed to reach the target. *)
    let presses =
      let dx = start.x - target.x
      and dy = start.y - target.y in
      let x = List.init (Int.abs dx) ~f:(Fn.const (if dx > 0 then '<' else '>'))
      and y = List.init (Int.abs dy) ~f:(Fn.const (if dy > 0 then '^' else 'v')) in
      List.append x y
    in
    (* Generate all permutations of the button presses, deduplicated, and
       filtered to the ones that are valid (i.e. does not cross over any empty
       spaces on the keypad). *)
    let valid_presses =
      let valid_vecs = Map.data pad |> Set.of_list (module Vec) in
      presses
      |> permutations
      |> List.dedup_and_sort ~compare:(List.compare Char.compare)
      |> List.filter ~f:(fun presses ->
        presses
        |> List.map ~f:Vec.of_dir
        |> List.folding_map ~init:start ~f:(fun v dir ->
          let v' = Vec.add v dir in
          v', v')
        |> List.map ~f:(Set.mem valid_vecs)
        |> List.fold ~init:true ~f:( && ))
    in
    (* Take each permutation and search recursively on how much it would cost
       to run against the rest of the keypad stack, and minimize over these
       values. *)
    let min =
      valid_presses
      |> List.map ~f:(fun presses ->
        memo_cost ~f:cost (presses @ [ 'A' ]) stack' dirpad_start)
      |> List.min_elt ~compare:Int.compare
      |> Option.value
           ~default:1 (* No presses means that this is a press, which costs 1 *)
    in
    (* Recurse through the rest of the sequence we want to enter. *)
    min + cost tl stack target
;;

let solve_stack input stack =
  input
  |> String.split_lines
  |> List.map ~f:(fun code ->
    let seq_len =
      memo_cost ~f:cost (String.to_list code) stack (Map.find_exn numpad 'A')
    in
    let num = String.chop_suffix_exn code ~suffix:"A" |> Int.of_string in
    num * seq_len)
  |> List.fold ~init:0 ~f:( + )
;;

let solve_part1 input =
  let stack = [ numpad; dirpad; dirpad ] in
  solve_stack input stack
;;

let solve_part2 input =
  let stack = [ numpad ] @ List.init 25 ~f:(Fn.const dirpad) in
  solve_stack input stack
;;

let solve _input =
  let part1 = solve_part1 _input in
  printf "part1: %d\n" part1;
  let part2 = solve_part2 _input in
  printf "part2: %d\n" part2
;;
