let leap_year n =
  match n mod 4 with
  | 0 when n mod 400 = 0 -> true
  | 0 when n mod 100 = 0 -> false
  | 0 -> true
  | _ -> false
