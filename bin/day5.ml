let parse s =
  match Str.split (Str.regexp "^$") s with
  | [ valid_s; ingredients_s ] ->
      ( List.map
          (fun line ->
            match String.split_on_char '-' line |> List.map int_of_string with
            | [ x; y ] -> (x, y)
            | _ -> failwith "Ranges must have exactly two elements")
          (String.trim valid_s |> String.split_on_char '\n'),
        List.map
          (fun line -> int_of_string line)
          (String.trim ingredients_s |> String.split_on_char '\n') )
  | _ -> failwith "Invalid input"

let in_range x (min, max) = min <= x && x <= max
let _ranges = [ (3, 5); (10, 14); (16, 20); (12, 18) ]

let merge_ranges ranges =
  let ranges = List.sort compare ranges in
  List.fold_left
    (fun acc (min2, max2) ->
      match acc with
      | (min1, max1) :: ranges ->
          if max1 < min2 then (min2, max2) :: (min1, max1) :: ranges
          else (min min1 min2, max max1 max2) :: ranges
      | [] -> [ (min2, max2) ])
    [] ranges

let () =
  let ranges, tests = parse (In_channel.input_all stdin) in
  let n =
    List.fold_left
      (fun acc x -> if List.exists (in_range x) ranges then acc + 1 else acc)
      0 tests
  in
  let n_ranges =
    merge_ranges ranges
    |> List.fold_left (fun acc (min1, max1) -> acc + 1 + max1 - min1) 0
  in
  Printf.printf "%d\n%d\n%!" n n_ranges
