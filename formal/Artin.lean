import Mathlib

open Function Fintype Subgroup Ideal Polynomial Submodule Zsqrtd
open scoped BigOperators
noncomputable section

theorem exercise_2_2_9 {G : Type*} [Group G] {a b : G}
  (h : a * b = b * a) :
  ∀ x y : closure {x | x = a ∨ x = b}, x*y = y*x :=
sorry

theorem exercise_2_3_2 {G : Type*} [Group G] (a b : G) :
  ∃ g : G, b* a = g * a * b * g⁻¹ :=
sorry

theorem exercise_2_4_19 {G : Type*} [Group G] {x : G}
  (hx : orderOf x = 2) (hx1 : ∀ y, orderOf y = 2 → y = x) :
  x ∈ center G :=
sorry

theorem exercise_2_8_6 {G H : Type*} [Group G] [Group H] :
  center (G × H) ≃* (center G) × (center H) :=
sorry

theorem exercise_2_11_3 {G : Type*} [Group G] [Fintype G]
  (hG : Even (card G)) : ∃ x : G, orderOf x = 2 :=
sorry

theorem exercise_3_2_7 {F : Type*} [Field F] {G : Type*} [Field G]
  (φ : F →+* G) : Injective φ :=
sorry

theorem exercise_3_5_6 {K V : Type*} [Field K] [AddCommGroup V]
  [Module K V] {S : Set V} (hS : Set.Countable S)
  (hS1 : span K S = ⊤) {ι : Type*} (R : ι → V)
  (hR : LinearIndependent K R) : Countable ι :=
sorry

theorem exercise_3_7_2 {K V : Type*} [Field K] [AddCommGroup V]
  [Module K V] {ι : Type*} [Fintype ι] (γ : ι → Submodule K V)
  (h : ∀ i : ι, γ i ≠ ⊤) :
  (⋂ (i : ι), (γ i : Set V)) ≠ ⊤ :=
sorry

theorem exercise_6_1_14 (G : Type*) [Group G]
  (hG : IsCyclic $ G ⧸ (center G)) :
  center G = ⊤  :=
sorry

theorem exercise_6_4_2 {G : Type*} [Group G] [Fintype G] {p q : ℕ}
  (hp : Prime p) (hq : Prime q) (hG : card G = p*q) :
  IsSimpleGroup G → false :=
sorry

theorem exercise_6_4_3 {G : Type*} [Group G] [Fintype G] {p q : ℕ}
  (hp : Prime p) (hq : Prime q) (hG : card G = p^2 *q) :
  IsSimpleGroup G → false :=
sorry

theorem exercise_6_4_12 {G : Type*} [Group G] [Fintype G]
  (hG : card G = 224) :
  IsSimpleGroup G → false :=
sorry

theorem exercise_6_8_1 {G : Type*} [Group G]
  (a b : G) : closure ({a, b} : Set G) = Subgroup.closure {b*a*b^2, b*a*b^3} :=
sorry

theorem exercise_10_1_13 {R : Type*} [Ring R] {x : R}
  (hx : IsNilpotent x) : IsUnit (1 + x) :=
sorry

theorem exercise_10_2_4 :
  span ({2} : Set $ Polynomial ℤ) ⊓ (span {X}) =
  span ({2 * X} : Set $ Polynomial ℤ) :=
sorry

theorem exercise_10_6_7 {I : Ideal GaussianInt}
  (hI : I ≠ ⊥) : ∃ (z : I), z ≠ 0 ∧ (z : GaussianInt).im = 0 :=
sorry

theorem exercise_10_4_6 {R : Type*} [CommRing R]
  [NoZeroDivisors R] (I J : Ideal R) (x : ↑(I ⊓ J)) :
  IsNilpotent ((Ideal.Quotient.mk (I*J)) x) :=
sorry

theorem exercise_10_4_7a {R : Type*} [CommRing R] [NoZeroDivisors R]
  (I J : Ideal R) (hIJ : I + J = ⊤) : I * J = I ⊓ J :=
sorry

theorem exercise_10_7_10 {R : Type*} [Ring R]
  (M : Ideal R) (hM : ∀ (x : R), x ∉ M → IsUnit x)
  (hProper : ∃ x : R, x ∉ M) :
  IsMaximal M ∧ ∀ (N : Ideal R), IsMaximal N → N = M :=
sorry

theorem exercise_11_2_13 (a b : ℤ) :
  (ofInt a : GaussianInt) ∣ ofInt b → a ∣ b :=
sorry

theorem exercise_11_4_1b {F : Type*} [Field F] [Fintype F] (hF : card F = 2) :
  Irreducible (12 + 6 * X + X ^ 3 : Polynomial F) :=
sorry

theorem exercise_11_4_6a {F : Type*} [Field F] [Fintype F] (hF : card F = 7) :
  Irreducible (X ^ 2 + 1 : Polynomial F) :=
sorry

theorem exercise_11_4_6b {F : Type*} [Field F] [Fintype F] (hF : card F = 31) :
  Irreducible (X ^ 3 - 9 : Polynomial F) :=
sorry

theorem exercise_11_4_6c : Irreducible (X^3 - 9 : Polynomial (ZMod 31)) :=
sorry

theorem exercise_11_4_8 (p : ℕ) (hp : Prime p) (n : ℕ) :
  -- p ∈ ℕ can be written as p ∈ ℚ[X]
  Irreducible (X ^ n - (p : Polynomial ℚ) : Polynomial ℚ) :=
sorry

theorem exercise_11_13_3 (N : ℕ):
  ∃ p ≥ N, Nat.Prime p ∧ p + 1 ≡ 0 [MOD 4] :=
sorry

theorem exercise_13_4_10
    {p : ℕ} {hp : Nat.Prime p} (h : ∃ r : ℕ, p = 2 ^ r + 1) :
    ∃ (k : ℕ), p = 2 ^ (2 ^ k) + 1 :=
sorry

theorem exercise_13_6_10 {K : Type*} [Field K] [Fintype Kˣ] :
  (∏ x : Kˣ,  x) = -1 :=
sorry
