module Coord = struct
  type t = int * int

  let compare = compare
end

module Cset = Set.Make (Coord)

let of_string s =
  let lines = String.split_on_char '\n' s in
  let entries =
    List.mapi
      (fun y line ->
        List.mapi
          (fun x c -> if c = '@' then Some (y, x) else None)
          (String.to_seq line |> List.of_seq))
      lines
    |> List.concat
  in
  let cset =
    List.fold_left
      (fun acc e ->
        match e with Some coord -> Cset.add coord acc | None -> acc)
      Cset.empty entries
  in
  cset

let n_rolls_around y x cset =
  List.fold_left
    (fun acc y_diff ->
      acc
      + List.fold_left
          (fun acc x_diff ->
            let y2 = y + y_diff in
            let x2 = x + x_diff in
            if (x2, y2) = (x, y) then acc
            else if Cset.mem (y2, x2) cset then acc + 1
            else acc)
          0 [ -1; 0; 1 ])
    0 [ -1; 0; 1 ]

let rolls_accessable cset =
  Cset.fold
    (fun (y, x) acc ->
      if n_rolls_around y x cset < 4 then Cset.add (y, x) acc else acc)
    cset Cset.empty

let n_rolls_accessable cset = rolls_accessable cset |> Cset.cardinal

let removable2 cset =
  let rec loop acc cset =
    let rset = rolls_accessable cset in
    if Cset.is_empty rset then acc
    else loop (Cset.cardinal rset + acc) (Cset.diff cset rset)
  in
  loop 0 cset

let () =
  let cset = of_string (In_channel.input_all stdin) in
  Printf.printf "%d\n%d\n%!" (n_rolls_accessable cset) (removable2 cset)
