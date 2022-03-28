
module type Nat 
  type Nat = Z | S Nat;
  val double : Nat -> Nat;
end

module impl Nat
  let double n =
    match n with 
      | \Z -> Z;
      | \(S x) -> S (S (double x));
end