let compose f g = fun x -> f x |> g
let (>>) = compose

let square x = x * x

(* Sum of an arithmetic series, e.g. https://courses.lumenlearning.com/ivytech-collegealgebra/chapter/using-the-formula-for-arithmetic-series/ *)
let sum_n n = n * (1 + n) / 2

let square_of_sum = sum_n >> square

(* https://en.wikipedia.org/wiki/Square_pyramidal_number#Formula *)
let sum_of_squares n = n * (n + 1) * (2 * n + 1) / 6

let difference_of_squares n =
    let square_of_sum = square_of_sum n in
    let sum_of_squares = sum_of_squares n in
    square_of_sum - sum_of_squares
