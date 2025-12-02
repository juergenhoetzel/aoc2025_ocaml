let rot_pattern = Str.regexp {|^\([LR]\)\([0-9]+\)$|}

let parse =
  List.filter_map (fun s ->
      if Str.string_match rot_pattern s 0 then
        let x = int_of_string (Str.matched_group 2 s) in
        Some (if Str.matched_group 1 s = "L" then -x else x)
      else None)

let dial rotations =
  List.fold_left (fun acc x -> (List.hd acc + x) :: acc) [ 50 ] rotations

(* Solve part2 in terms of part1 *)
let dial_single_step =
 fun x -> List.init (abs x) (fun _ -> if x < 0 then -1 else 1)

let () =
  let steps = In_channel.input_lines stdin |> parse in
  let dials = dial steps in
  let dials_single_step =
    steps |> List.map dial_single_step |> List.concat |> dial
  in
  List.iter
    (fun dials ->
      List.filter (fun x -> x mod 100 = 0) dials
      |> List.length |> Format.printf "%d\n")
    [ dials; dials_single_step ]
