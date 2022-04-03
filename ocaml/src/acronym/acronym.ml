open Base

let compose f g = fun x -> f x |> g
let (>>) = compose

let word_delimiters = [' '; '-'; ',' ] 

let is_word_delimiter = List.mem word_delimiters ~equal:Char.equal

let acronym =
  String.fold ~init:("", true) ~f:(fun (acc, take_next_alpha_char) c ->
    match c with
    | c when is_word_delimiter c -> acc, true
    | c when Char.is_alpha c && take_next_alpha_char -> acc ^ (Char.uppercase c |> String.of_char), false
    | _ -> acc, take_next_alpha_char
    )
    >> fst
