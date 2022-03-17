
module sig Nat
  type Nat = Z | S Nat
  fun double : Nat -> Nat
end 

module Nat impl
  let double n =
    match n with 
      | Z -> Z
      | S x -> S (S (double x))
end