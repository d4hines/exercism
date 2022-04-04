type allergen =
  | Eggs
  | Peanuts
  | Shellfish
  | Strawberries
  | Tomatoes
  | Chocolate
  | Pollen
  | Cats

(* Order matters for the tests *)
let allergens =
  [ Eggs; Peanuts; Shellfish; Strawberries; Tomatoes; Chocolate; Pollen; Cats ]

let allergen_to_score = function
  | Eggs -> 1
  | Peanuts -> 2
  | Shellfish -> 4
  | Strawberries -> 8
  | Tomatoes -> 16
  | Chocolate -> 32
  | Pollen -> 64
  | Cats -> 128

let nth_bit n i = Int.equal (i land n) n
let allergic_to score allergen = nth_bit (allergen_to_score allergen) score
let allergies score = List.filter (allergic_to score) allergens
