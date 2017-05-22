let S n s z = s n ( n s z ) in
let Z s z = z in

type rec ℕ of
       | S : ℕ → ℕ
       | Z : ℕ
  =
  ∀ P : Nat → ⋆ . ( Π n : Nat . P n → P ( S n ) ) → P Z → P self
in
  
    
let ~::~ len head tail ~::~ [] = head :: tail ( tail [] ~::~ ) in
let [] ~::~ [] = [] in

type Vector (A : ⋆) : (n : ℕ) of
   | ~::~ : ∀ n : Nat . A → Vector n → Vector (S n)
   | [] : Vector Z
  =
  ∀ P : Π n : ℕ . Vector n → ⋆ .
  ( ∀ n : ℕ . Π h : A . Π t : Vector n . P n t → P ( S n ) ( h :: t ) ) →
  ( P Z [] ) →
  ( P n self )
in

let vector_map ( f : A → B ) ( vec n : Vector A ) : Vector B n =
  match vec with
  | [] -> []
  | h :: t -> λ r . ( f a ) :: r                     
in

Vector

    

      
