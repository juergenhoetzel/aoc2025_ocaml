let rot_pattern = Str.regexp {|^\([LR]\)\([0-9]+\)$|}

let parse =
  List.filter_map (fun s ->
      if Str.string_match rot_pattern s 0 then
        let x = int_of_string (Str.matched_group 2 s) in
        Some (if Str.matched_group 1 s = "L" then -x else x)
      else None)

let dial rotations =
  List.fold_left
    (fun acc x -> ((List.hd acc + x + 100) mod 100) :: acc)
    [ 50 ] rotations

let () =
  let n =
    In_channel.input_lines stdin
    |> parse |> dial
    |> List.filter (fun x -> x = 0)
    |> List.length
  in
  Format.printf "%d\n%!" n
