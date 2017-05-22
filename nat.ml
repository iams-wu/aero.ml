let S n s z = s n ( n s z ) in
let Z s z = z in

type rec ℕ of
       | S : ℕ → ℕ
       | Z : ℕ
  =
  ∀ P : Nat → ⋆ . ( Π n : Nat . P n → P ( S n ) ) → P Z → P self
in

ℕ
                          
