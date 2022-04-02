let list_of_n n = 
    let rec go n l = match n with
    | 0 -> l
    | n -> go (n - 1) (n :: l)
in
go n []

let sum_of_list = List.fold_left (+) 0

let square x = x * x

let compose f g = fun x -> f x |> g
let (>>) = compose

let square_of_sum x = 
    list_of_n x |> sum_of_list |> square
let sum_of_squares n = list_of_n n |> List.map square |> sum_of_list

let difference_of_squares n =
    let square_of_sum = square_of_sum n in
    let sum_of_squares = sum_of_squares n in
    square_of_sum - sum_of_squares
