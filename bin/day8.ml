let distance (x1, y1, z1) (x2, y2, z2) =
  let x_diff = x1 - x2 in
  let y_diff = y1 - y2 in
  let z_diff = z1 - z2 in
  (x_diff * x_diff) + (y_diff * y_diff) + (z_diff * z_diff)

let distances xs =
  let rec loop acc = function
    | [] | [ _ ] -> acc
    | x :: ys ->
        let distances_from_x =
          List.fold_left (fun acc y -> (distance x y, x, y) :: acc) [] ys
        in
        loop (distances_from_x @ acc) ys
  in
  let all_distances = loop [] xs in
  List.sort (fun (d1, _, _) (d2, _, _) -> compare d1 d2) all_distances

let parse s =
  String.trim s |> String.split_on_char '\n'
  |> List.map (fun line ->
      String.split_on_char ',' line |> List.map int_of_string |> function
      | [ x; y; z ] -> (x, y, z)
      | _ -> invalid_arg line)

module Pset = Set.Make (struct
  type t = int * int * int

  let compare = compare
end)

module Pmap = Map.Make (struct
  type t = int * int * int

  let compare = compare
end)

module Sset = Set.Make (Pset)

let update_mapping pmap (point1, point2) =
  let pset =
    match (Pmap.find_opt point1 pmap, Pmap.find_opt point2 pmap) with
    | None, None -> Pset.of_list [ point1; point2 ]
    | Some pset, None -> Pset.add point2 pset
    | None, Some pset -> Pset.add point1 pset
    | Some pset1, Some pset2 when pset1 = pset2 -> pset2
    | Some pset1, Some pset2 -> Pset.union pset1 pset2
  in
  List.fold_left
    (fun pmap point -> Pmap.update point (fun _ -> Some pset) pmap)
    pmap (Pset.to_list pset)

let buckets distances =
  let pmap = Pmap.empty in
  List.fold_left
    (fun pmap (_, point1, point2) -> update_mapping pmap (point1, point2))
    pmap distances

let final_connection circuit_length distances =
  let pmap = Pmap.empty in
  let rec loop pmap distances =
    match distances with
    | (_, point1, point2) :: distances2 ->
        let pmap = update_mapping pmap (point1, point2) in
        if
          Pmap.to_list pmap |> List.map snd
          |> List.exists (fun pset -> Pset.cardinal pset = circuit_length)
        then (point1, point2)
        else loop pmap distances2
    | [] -> failwith "Can't connect all circuits"
  in
  loop pmap distances

let part1 limit distances =
  distances |> List.take limit |> buckets |> Pmap.to_list |> List.map snd
  |> Sset.of_list |> Sset.to_list |> List.map Pset.to_list
  |> List.map List.length
  |> List.sort (fun a b -> Int.compare b a)
  |> List.take 3 |> List.fold_left ( * ) 1

let part2 points distances =
  let circuit_length = List.length points in
  let (x, _, _), (y, _, _) = final_connection circuit_length distances in
  x * y

let () =
  let limit =
    if Array.length Sys.argv = 2 then Sys.argv.(1) |> int_of_string else 1000
  in
  let points = In_channel.input_all stdin |> parse in
  let distances = distances points in
  part1 limit distances |> Printf.printf "%d\n%!";
  part2 points distances |> Printf.printf "%d\n%!"
