import Mathlib

open Real
open scoped BigOperators
noncomputable section

theorem exercise_1_27 {n : ℕ} (hn : Odd n) : 8 ∣ (n^2 - 1) :=
sorry

theorem exercise_1_30 {n : ℕ} :
  ¬ ∃ a : ℤ, ∑ i : Fin n, (1 : ℚ) / (n+2) = a :=
sorry

theorem exercise_1_31  : (⟨1, 1⟩ : GaussianInt) ^ 2 ∣ 2 :=
sorry

theorem exercise_2_4 {a : ℤ} (ha : a ≠ 0)
  (f_a := λ n m : ℕ => Int.gcd (a^(2^n) + 1) (a^(2^m)+1)) {n m : ℕ}
  (hnm : n > m) :
  (Odd a → f_a n m = 1) ∧ (Even a → f_a n m = 2) :=
sorry

theorem exercise_2_21 {l : ℕ → ℝ}
  (hl : ∀ p n : ℕ, p.Prime → l (p^n) = log p )
  (hl1 : ∀ m : ℕ, ¬ IsPrimePow m → l m = 0) :
  l = λ n => ∑ d : Nat.divisors n, ArithmeticFunction.moebius (n/d) * log d  :=
sorry

theorem exercise_2_27a :
  ¬ Summable (λ i : {p : ℤ // Squarefree p} => (1 : ℚ) / i) :=
sorry

theorem exercise_3_1 : Infinite {p : Nat.Primes // p ≡ -1 [ZMOD 6]} :=
sorry

theorem exercise_3_4 : ¬ ∃ x y : ℤ, 3*x^2 + 2 = y^2 :=
sorry

theorem exercise_3_5 : ¬ ∃ x y : ℤ, 7*x^3 + 2 = y^3 :=
sorry

theorem exercise_3_10 {n : ℕ} (hn0 : ¬ n.Prime) (hn1 : n ≠ 4) :
  Nat.factorial (n-1) ≡ 0 [MOD n] :=
sorry

theorem exercise_3_14 {p q n : ℕ} (hp0 : p.Prime ∧ p > 2)
  (hq0 : q.Prime ∧ q > 2) (hpq0 : p ≠ q) (hpq1 : p - 1 ∣ q - 1)
  (hn : n.gcd (p*q) = 1) :
  n^(q-1) ≡ 1 [MOD p*q] :=
sorry

theorem exercise_4_4 {p t: ℕ} (hp0 : p.Prime) (hp1 : p = 4*t + 1)
  (a : ZMod p) :
  IsPrimitiveRoot a p ↔ IsPrimitiveRoot (-a) p :=
sorry

theorem exercise_4_5 {p t : ℕ} (hp0 : p.Prime) (hp1 : p = 4*t + 3)
  (a : ZMod p) :
  IsPrimitiveRoot a p ↔ ((-a) ^ ((p-1)/2) = 1 ∧ ∀ (k : ℕ), k < (p-1)/2 → (-a)^k ≠ 1) :=
sorry

theorem exercise_4_6 {p n : ℕ} (hp : p.Prime) (hpn : p = 2^n + 1) :
  IsPrimitiveRoot 3 p :=
sorry

theorem exercise_4_8 {p a : ℕ} (hp : Odd p) :
  IsPrimitiveRoot a p ↔ (∀ q : ℕ, q ∣ (p-1) → q.Prime → ¬ a^(p-1) ≡ 1 [MOD p]) :=
sorry

theorem exercise_4_11 {p : ℕ} (hp : p.Prime) (k s: ℕ)
  (s := ∑ n : Fin p, (n : ℕ) ^ k) :
  ((¬ p - 1 ∣ k) → s ≡ 0 [MOD p]) ∧ (p - 1 ∣ k → s ≡ 0 [MOD p]) :=
sorry

theorem exercise_5_13 {p x: ℤ} (hp : Prime p)
  (hpx : p ∣ (x^4 - x^2 + 1)) : p ≡ 1 [ZMOD 12] :=
sorry

theorem exercise_5_28 {p : ℕ} (hp : p.Prime) (hp1 : p ≡ 1 [MOD 4]):
  ∃ x, x^4 ≡ 2 [MOD p] ↔ ∃ A B, p = A^2 + 64*B^2 :=
sorry

theorem exercise_5_37 {p q : ℕ} [Fact (p.Prime)] [Fact (q.Prime)] {a : ℤ}
  (ha : a < 0) (h0 : p ≡ q [ZMOD 4*a]) (h1 : ¬ ((p : ℤ) ∣ a)) :
  legendreSym p a = legendreSym q a :=
sorry

theorem exercise_12_12 : IsAlgebraic ℚ (sin (pi/12)) :=
sorry

theorem exercise_18_4 {n : ℕ} (hn : ∃ x y z w : ℤ,
  x^3 + y^3 = n ∧ z^3 + w^3 = n ∧ x ≠ z ∧ x ≠ w ∧ y ≠ z ∧ y ≠ w) :
  n ≥ 1729 :=
sorry
