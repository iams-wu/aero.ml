let S n s z = s n ( n s z ) in
let Z s z = z in

type rec ℕ of
       | S : ℕ → ℕ
       | Z : ℕ
  =
  ∀ P : Nat → ⋆ . ( Π n : Nat . P n → P ( S n ) ) → P Z → P self
in

let S-inj ( x : ℕ ) ( y : ℕ ) ( hypothesis : S x ≡ S y ) : x ≡ y =
  π1 ( ε hypothesis )
in

let add-S-comm-0 ( x : ℕ ) ( m : ℕ ) : add ( S n ) m ≡ S ( add n m ) =
  λ _ . λ _ . β
in

let add-S-comm-1 ( x : ℕ ) ( m : ℕ ) : add n ( S m ) ≡ S ( add n m ) =
  match n with
  | Z -> ̱β
  | S _ cx -> ε1 ρ cx − β
in

let add-S-comm ( x : ℕ ) ( y : ℕ ) : add ( S x ) ( S y ) ≡ S ( S ( add x y ) ) =
  ρ add-S-comm-0 x ( S y ) − ρ ( add-S-comm-1 x y ) − β
in

let add-inj ( x : ℕ ) ( y : ℕ ) ( z : ℕ ) ( hyp : add x y ≡ add x z ) : y ≡ z =
  match x with
  | Z → ρ ( add-ident 0 y ) − ρ ( add-ident-0 z ) − pf
  | S x cx →
      let hyp' : add ( S x ) z ≡ suc ( add x z ) = add-S-comm 0 x z in
      let hyp'' : add ( S x ) y ≡ suc ( add x y ) = add-S-comm 0 x y in
      cx ( S-inj ( add x y ) ( add x z ) ( ρ hyp'' − ρ hyp' − hyp )
in

( λ _ x . x )
