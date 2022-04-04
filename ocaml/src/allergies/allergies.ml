type allergen =
  | Eggs
  | Peanuts
  | Shellfish
  | Strawberries
  | Tomatoes
  | Chocolate
  | Pollen
  | Cats

let allergenScores =
  [
    (Eggs, 1);
    (Peanuts, 2);
    (Shellfish, 4);
    (Strawberries, 8);
    (Tomatoes, 16);
    (Chocolate, 32);
    (Pollen, 64);
    (Cats, 128);
  ]

let nth_bit n i = Int.equal (i land n) n

let allergic_to score allergen =
  let _, n = List.find (fun (a, _) -> a = allergen) allergenScores in
  nth_bit n score

let allergies score =
  List.fold_right
    (fun (allergen, n) acc -> if nth_bit n score then allergen :: acc else acc)
    allergenScores []
