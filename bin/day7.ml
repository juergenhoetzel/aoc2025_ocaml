(* FIXME: Move to lib *)
module Coord = struct
  type t = int * int

  let compare = compare
end

module Positions = Set.Make (Coord)

let parse s =
  let beam_positions = Positions.empty in
  let splitter_positions = Positions.empty in
  Seq.fold_left
    (fun (splitter_positions, beam_positions, y, x) c ->
      match c with
      | '\n' -> (splitter_positions, beam_positions, y + 1, 0)
      | 'S' ->
          (splitter_positions, Positions.add (y, x) beam_positions, y, x + 1)
      | '^' ->
          (Positions.add (y, x) splitter_positions, beam_positions, y, x + 1)
      | '.' -> (splitter_positions, beam_positions, y, x + 1)
      | _ -> invalid_arg s)
    (splitter_positions, beam_positions, 0, 0)
    (String.to_seq s)

let move splitter_positions beam_positions =
  Positions.to_list beam_positions
  |> List.fold_left
       (fun (new_beam_positions, splits) (y, x) ->
         if Positions.mem (y + 1, x) splitter_positions then
           ( Positions.add (y + 1, x + 1) new_beam_positions
             |> Positions.add (y + 1, x - 1),
             splits + 1 )
         else (Positions.add (y + 1, x) new_beam_positions, splits))
       (Positions.empty, 0)

let () =
  let splitter_positions, beam_positions, y_size, _ =
    parse (In_channel.input_all stdin)
  in
  let rec loop acc beam_positions =
    let beam_positions, splits = move splitter_positions beam_positions in
    if Positions.to_list beam_positions |> List.hd |> fst > y_size then acc
    else loop (acc + splits) beam_positions
  in
  let splits = loop 0 beam_positions in
  Printf.printf "%d\n%!" splits
