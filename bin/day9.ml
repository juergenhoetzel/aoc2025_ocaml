let size (x, y) (x2, y2) = (abs (x - x2) + 1) * (abs (y - y2) + 1)

let rec max_size_positions positions max_size =
  match positions with
  | [] | [ _ ] -> max_size
  | x :: xs ->
      let max_size =
        List.fold_left
          (fun max_size x2 -> Int.max (size x x2) max_size)
          max_size xs
      in
      max_size_positions xs max_size

let parse = List.map (fun s -> Scanf.sscanf s "%d,%d" (fun x y -> (x, y)))

let () =
  let positions = In_channel.input_lines stdin |> parse in
  let max_square = max_size_positions positions 0 in
  Printf.printf "%d\n%!" max_square
