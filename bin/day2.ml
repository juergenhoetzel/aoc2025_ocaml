module IntSet = Set.Make (Int)

let invalid x n =
  let s = string_of_int x in
  List.init n (Fun.const s) |> String.concat "" |> int_of_string

let pow base exp =
  let rec loop acc = function 0 -> acc | n -> loop (acc * base) (n - 1) in
  loop 1 exp

let invalid_in_range replicate_multiple x y =
  let e = Float.log10 (float_of_int y) |> Float.ceil |> Int.of_float in
  let y_scan = y / pow 10 (e / 2) in
  let rec loop acc i =
    if i > y_scan then acc
    else
      let rec invalid_loop (invalids : IntSet.t) n =
        let iv = invalid i n in
        let new_invalids =
          if x <= iv && iv <= y then IntSet.add iv invalids else invalids
        in
        if replicate_multiple then
          if iv <= y then invalid_loop new_invalids (n + 1) else invalids
        else new_invalids
      in
      let ivs = invalid_loop IntSet.empty 2 in
      loop (IntSet.union acc ivs) (i + 1)
  in
  loop IntSet.empty 1 |> IntSet.to_list

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
    List.map (fun (x, y) -> invalid_in_range false x y) ranges
    |> List.concat |> List.fold_left ( + ) 0
  in
  let n2 =
    List.map (fun (x, y) -> invalid_in_range true x y) ranges
    |> List.concat |> List.fold_left ( + ) 0
  in
  Printf.printf "%d\n%d\n%!" n n2
