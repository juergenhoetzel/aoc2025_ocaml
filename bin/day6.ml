let parse s =
  let lines = String.trim s |> String.split_on_char '\n' in
  let n = List.length lines in
  let operations = List.nth lines (n - 1) |> Str.split (Str.regexp " +") in
  let rows =
    List.map
      (fun line -> Str.split (Str.regexp " +") line |> List.map int_of_string)
      (List.take (n - 1) lines)
  in
  (rows, operations)

let fold_numbers rows operations =
  List.mapi
    (fun x operation ->
      let fn =
        match operation with
        | "+" -> ( + )
        | "*" -> ( * )
        | _ -> invalid_arg operation
      in
      List.fold_left
        (fun acc row -> fn (List.nth row x) acc)
        (List.nth (List.hd rows) x)
        (List.tl rows))
    operations

let () =
  let s = In_channel.input_all stdin in
  let rows, operations = parse s in
  Printf.printf "%d\n%!" (fold_numbers rows operations |> List.fold_left ( + ) 0)
