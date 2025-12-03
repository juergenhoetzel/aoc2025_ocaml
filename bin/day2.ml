let invalid x = Printf.sprintf "%d%d" x x |> int_of_string

let pow base exp =
  let rec loop acc = function 0 -> acc | n -> loop (acc * base) (n - 1) in
  loop 1 exp

let invalid_in_range x y =
  let e = (Float.log10 (float_of_int x) |> Float.ceil |> Int.of_float) + 1 in
  let x_scan = x / pow 10 (e / 2) in
  let e = Float.log10 (float_of_int y) |> Float.ceil |> Int.of_float in
  let y_scan = y / pow 10 (e / 2) in
  let rec loop acc i =
    if i > y_scan then acc
    else
      let iv = invalid i in
      if x <= iv && iv <= y then loop (iv :: acc) (i + 1) else loop acc (i + 1)
  in
  loop [] x_scan

let parse s =
  List.filter_map
    (fun s ->
      match String.split_on_char '-' s with
      | [ x; y ] -> Some (int_of_string x, int_of_string y)
      | _ -> None)
    (String.split_on_char ',' s)

let () =
  let ranges = In_channel.input_all stdin |> String.trim |> parse in
  let n =
    List.map (fun (x, y) -> invalid_in_range x y) ranges
    |> List.concat |> List.fold_left ( + ) 0
  in
  Printf.printf "%d\n%!" n
