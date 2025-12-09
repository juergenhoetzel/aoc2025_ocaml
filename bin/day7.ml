module IntSet = Set.Make (Int)
module IntMap = Map.Make (Int)

let parse s =
  let lines = String.split_on_char '\n' s in
  let beams =
    String.to_seq (List.hd lines)
    |> Seq.mapi (fun x c -> if c = 'S' then (x, 1) else (x, 0))
    |> IntMap.of_seq
  in
  let splitters =
    List.map
      (fun line ->
        String.to_seq line
        |> Seq.mapi (fun x c -> if c = '^' then Some x else None)
        |> Seq.filter_map Fun.id |> IntSet.of_seq)
      (List.tl lines)
  in
  (beams, splitters)

let move splitters beams =
  let new_counts =
    List.map
      (fun (x, count) ->
        if IntSet.mem x splitters then [ (x - 1, count); (x + 1, count) ]
        else [ (x, count) ])
      (IntMap.to_list beams)
    |> List.concat
  in
  ( List.fold_left
      (fun beams (x, count) ->
        IntMap.update x
          (function None -> Some count | Some count2 -> Some (count + count2))
          beams)
      IntMap.empty new_counts,
    List.length new_counts - IntMap.cardinal beams )

let () =
  let beams, splitters = parse (In_channel.input_all stdin) in
  let final_beams, splits =
    List.fold_left
      (fun (beams, splits) splitters ->
        let beams, new_splits = move splitters beams in
        (beams, new_splits + splits))
      (beams, 0) splitters
  in
  let timelines = IntMap.fold (fun _ c acc -> acc + c) final_beams 0 in
  Printf.printf "%d\n%d\n%!" splits timelines
