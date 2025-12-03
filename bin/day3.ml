let parse =
  List.map (fun s ->
      String.to_seq s
      |> Seq.map (fun c -> int_of_char c - int_of_char '0')
      |> List.of_seq)

let remove_first_asc xs z =
  let rec loop = function
    | x :: y :: xs -> if x < y then (y :: xs) @ [ z ] else x :: loop (y :: xs)
    | [ x ] -> if x > z then [ x ] else [ z ]
    | _ -> failwith "empty list xs"
  in
  loop xs

let largest_joltage n xs =
  let current = List.take n xs in
  let rst = List.drop n xs in
  let final = List.fold_left (fun xs z -> remove_first_asc xs z) current rst in
  List.fold_left (fun acc x -> (acc * 10) + x) 0 final

let () =
  let batteries = parse (In_channel.input_lines stdin) in
  let totals =
    List.map
      (fun n ->
        batteries |> List.map (largest_joltage n) |> List.fold_left ( + ) 0)
      [ 2; 12 ]
  in
  List.iter (Format.printf "%d\n%!") totals
