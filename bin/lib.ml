let rec lookup want list =
  match list with
  | [] -> failwith "Not_found"
  | (name, data) :: rest -> if name = want then data else lookup want rest

let diff xs ys = List.filter (fun x -> not (List.mem x ys)) xs
