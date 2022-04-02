type nucleotide = A | C | G | T

let hamming_distance a b =
  match (a, b) with
  | [], [] -> Ok 0
  | [], _ -> Error "left strand must not be empty"
  | _, [] -> Error "right strand must not be empty"
  | a, b when List.length a <> List.length b ->
      Error "left and right strands must be of equal length"
  | a, b ->
      let distance =
        List.fold_left
          (fun distance (a, b) -> if a = b then distance else distance + 1)
          0 (List.combine a b)
      in
      Ok distance
