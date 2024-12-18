open! Core

module Cpu = struct
  type t =
    { ra : int
    ; rb : int
    ; rc : int
    ; ip : int
    ; program : int list
    ; output : int list
    }

  let parser =
    Angstrom.(
      let open Let_syntax in
      let number = take_while1 Char.is_digit >>| Int.of_string in
      let register name =
        let* _ = string "Register " in
        let* _ = string name in
        let* _ = string ": " in
        let* value = number in
        return value
      in
      let program =
        let* _ = string "Program: " in
        let* program = sep_by1 (string ",") number in
        return program
      in
      let* ra = register "A" <* end_of_line in
      let* rb = register "B" <* end_of_line in
      let* rc = register "C" <* end_of_line in
      let* _ = end_of_line in
      let* program = program <* end_of_line in
      return { ra; rb; rc; ip = 0; program; output = [] })
  ;;

  let fetch_inst ?(offset = 0) t = List.nth t.program (t.ip + offset)
  let fetch_inst_exn ?(offset = 0) t = fetch_inst ~offset t |> Option.value_exn

  let step t =
    let combo op =
      match op with
      | 0 | 1 | 2 | 3 -> op
      | 4 -> t.ra
      | 5 -> t.rb
      | 6 -> t.rc
      | _ -> failwith "invalid combo op"
    in
    let ( let* ) a f = Option.bind a ~f in
    let* instr = fetch_inst t in
    match instr with
    | 0 ->
      (* ;; adv op
         %ra <- %ra >> (combo op)
         %ip <- %ip + $2 *)
      let* op = fetch_inst t ~offset:1 in
      let ra = t.ra lsr combo op in
      Some { t with ra; ip = t.ip + 2 }
    | 1 ->
      (* ;; bxl op
         %rb <- %rb ^ $op
         %ip <- %ip + $2 *)
      let* op = fetch_inst t ~offset:1 in
      let rb = Int.bit_xor t.rb op in
      Some { t with rb; ip = t.ip + 2 }
    | 2 ->
      (* ;; bst op
         %rb <- (combo op) % $8
         %ip <- %ip + $2 *)
      let* op = fetch_inst t ~offset:1 in
      let res = combo op % 8 in
      Some { t with rb = res; ip = t.ip + 2 }
    | 3 ->
      (* ;; jnz op
         %ip <- (%ra eq $0) : %ip + $2 ? $op *)
      let* op = fetch_inst t ~offset:1 in
      let ip = if t.ra = 0 then t.ip + 2 else op in
      Some { t with ip }
    | 4 ->
      (* ;; bxc _op
         %rb <- %rb ^ %rc
         %ip <- %ip + $2 *)
      let rb = Int.bit_xor t.rb t.rc in
      Some { t with rb; ip = t.ip + 2 }
    | 5 ->
      (* ;; out op
         %out += (combo op) % $8
         %ip <- %ip + $2 *)
      let* op = fetch_inst t ~offset:1 in
      let output = List.append t.output [ combo op % 8 ] in
      Some { t with output; ip = t.ip + 2 }
    | 6 ->
      (* ;; bdv op
         %rb <- %ra >> (combo op)
         %ip <- %ip + $2 *)
      let* op = fetch_inst t ~offset:1 in
      let rb = t.ra lsr combo op in
      Some { t with rb; ip = t.ip + 2 }
    | 7 ->
      (* ;; cdv op
         %rc <- %ra >> (combo op)
         %ip <- %ip + $2 *)
      let* op = fetch_inst t ~offset:1 in
      let rc = t.ra lsr combo op in
      Some { t with rc; ip = t.ip + 2 }
    | _ -> failwith "invalid program"
  ;;

  let rec run_until_halt t =
    match step t with
    | Some t' -> run_until_halt t'
    | None -> t
  ;;

  let to_assembly t =
    let lit n = "$" ^ Int.to_string n in
    let combo op =
      match op with
      | 0 | 1 | 2 | 3 -> lit op
      | 4 -> "%ra"
      | 5 -> "%rb"
      | 6 -> "%rc"
      | _ -> failwith "todo"
    in
    let rec loop prog out =
      match prog with
      | [] -> List.rev out
      | 0 :: op :: tl ->
        let line = "adv " ^ combo op in
        loop tl (line :: out)
      | 1 :: op :: tl ->
        let line = "bxl " ^ lit op in
        loop tl (line :: out)
      | 2 :: op :: tl ->
        let line = "bst " ^ combo op in
        loop tl (line :: out)
      | 3 :: op :: tl ->
        let line = "jnz " ^ lit op in
        loop tl (line :: out)
      | 4 :: _op :: tl ->
        let line = "bxc" in
        loop tl (line :: out)
      | 5 :: op :: tl ->
        let line = "out " ^ combo op in
        loop tl (line :: out)
      | 6 :: op :: tl ->
        let line = "bdv " ^ combo op in
        loop tl (line :: out)
      | 7 :: op :: tl ->
        let line = "cdv " ^ combo op in
        loop tl (line :: out)
      | _ -> failwith "todo"
    in
    loop t.program []
  ;;

  let output t = t.output |> List.map ~f:Int.to_string |> String.concat ~sep:","
end

let parse = Angstrom.(parse_string ~consume:Consume.All Cpu.parser)

let solve_part1 cpu =
  let cpu = Cpu.run_until_halt cpu in
  Cpu.output cpu
;;

let solve_part2 (cpu : Cpu.t) =
  let last_n list n =
    let rev = List.rev list in
    let last_n = List.take rev n in
    List.rev last_n
  in
  let prog_equal a b = List.equal Int.equal a b in
  let assembly = Cpu.to_assembly cpu in
  (* The program has a special form, where we start with a section of modifying
     %ra and %rb, then followed by a tail out

      out %rb  ;; output content of %rb
      adv $3   ;; %ra <- %ra >> $3
      jnz $0   ;; if %ra eq 0 then halt else loop from start

     Or in pseudocode:

      int ra, rb, rc
      do {
        prog(&ra, &rb, &rc)
        out += rb % 8
        ra = ra >> 3
      } while (ra != 0)
  *)
  (* Check to see if the program holds this form. *)
  let () =
    match last_n assembly 3 with
    | [ "out %rb"; "adv $3"; "jnz $0" ] -> ()
    | _ -> failwith "program does now follow expected form"
  in
  (* Because of this form we can simplify the search: the program runs in
     iterations of 3 bits on the %ra value, and only outputs once in each
     iteration. We can do a search through the reverse output in groups of 3
     bits: iter through all 3-bit combinations, run the program, and see if it
     outputs the same tail as the program. If so, bitshift the %ra value 3 to
     the left and check if we can continue finding values that match up with
     the program. *)
  let rec find ra : int list =
    List.init (1 lsl 3) ~f:Fn.id
    |> List.filter_map ~f:(fun i ->
      let ra = ra + i in
      let cpu = Cpu.run_until_halt { cpu with ra } in
      let prog' = last_n cpu.program (List.length cpu.output) in
      match () with
      (* Whole program matches -> valid value for %ra *)
      | () when prog_equal cpu.program cpu.output -> Some [ ra ]
      (* Program tail matches -> possible valid value, iter with %ra<<3. *)
      | () when prog_equal prog' cpu.output ->
        (match find (ra lsl 3) with
         | [] -> None
         | f -> Some f)
      (* Not a match, prune search tree. *)
      | () -> None)
    |> List.concat
  in
  find 0 |> List.min_elt ~compare:Int.compare |> Option.value_exn
;;

let solve input =
  let cpu = parse input |> Result.ok_or_failwith in
  let part1 = solve_part1 cpu in
  printf "part1: %s\n" part1;
  let part2 = solve_part2 cpu in
  printf "part2: %d\n" part2
;;
