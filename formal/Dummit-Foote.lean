import Mathlib

open Fintype Subgroup Set Polynomial Ideal
open scoped BigOperators
noncomputable section

theorem exercise_1_1_2a : ∃ a b : ℤ, a - b ≠ b - a :=
sorry

theorem exercise_1_1_3 (n : ℤ) :
  ∀ (a b c : ℤ), (a+b)+c ≡ a+(b+c) [ZMOD n] :=
sorry

theorem exercise_1_1_4 (n : ℕ) :
  ∀ (a b c : ℕ), (a * b) * c ≡ a * (b * c) [ZMOD n] :=
sorry

theorem exercise_1_1_5 (n : ℕ) (hn : 1 < n) :
  IsEmpty (Group (ZMod n)) :=
sorry

theorem exercise_1_1_15 {G : Type*} [Group G] (as : List G) :
  as.prod⁻¹ = (as.reverse.map (λ x => x⁻¹)).prod :=
sorry

theorem exercise_1_1_16 {G : Type*} [Group G]
  (x : G) (hx : x ^ 2 = 1) :
  orderOf x = 1 ∨ orderOf x = 2 :=
sorry

theorem exercise_1_1_17 {G : Type*} [Group G] {x : G} {n : ℕ}
  (hxn: orderOf x = n) :
  x⁻¹ = x ^ (n - 1 : ℤ) :=
sorry

theorem exercise_1_1_18 {G : Type*} [Group G]
  (x y : G) : (x * y = y * x ↔ y⁻¹ * x * y = x) ↔ (x⁻¹ * y⁻¹ * x * y = 1) :=
sorry

theorem exercise_1_1_20 {G : Type*} [Group G] {x : G} :
  orderOf x = orderOf x⁻¹ :=
sorry

theorem exercise_1_1_22a {G : Type*} [Group G] (x g : G) :
  orderOf x = orderOf (g⁻¹ * x * g) :=
sorry

theorem exercise_1_1_22b {G: Type*} [Group G] (a b : G) :
  orderOf (a * b) = orderOf (b * a) :=
sorry

theorem exercise_1_1_25 {G : Type*} [Group G]
  (h : ∀ x : G, x ^ 2 = 1) : ∀ a b : G, a*b = b*a :=
sorry

theorem exercise_1_1_29 {A B : Type*} [Group A] [Group B] :
  ∀ x y : A × B, x*y = y*x ↔ (∀ x y : A, x*y = y*x) ∧
  (∀ x y : B, x*y = y*x) :=
sorry

theorem exercise_1_1_34 {G : Type*} [Group G] {x : G}
  (hx_inf : orderOf x = 0) (n m : ℤ) :
  x ^ n ≠ x ^ m :=
sorry

theorem exercise_1_3_8 : Infinite (Equiv.Perm ℕ) :=
sorry

theorem exercise_1_6_4 :
  IsEmpty (Multiplicative ℝ ≃* Multiplicative ℂ) :=
sorry

theorem exercise_1_6_11 {A B : Type*} [Group A] [Group B] :
  A × B ≃* B × A :=
sorry

theorem exercise_1_6_17 {G : Type*} [Group G] (f : G → G)
  (hf : f = λ g => g⁻¹) :
  ∀ x y : G, f x * f y = f (x*y) ↔ ∀ x y : G, x*y = y*x :=
sorry

theorem exercise_1_6_23 {G : Type*}
  [Group G] (σ : MulAut G) (hs : ∀ g : G, σ g = 1 → g = 1)
  (hs2 : ∀ g : G, σ (σ g) = g) :
  ∀ x y : G, x*y = y*x :=
sorry

theorem exercise_2_1_5 {G : Type*} [Group G] [Fintype G]
  (hG : card G > 2) (H : Subgroup G) [Fintype H] :
  card H ≠ card G - 1 :=
sorry

theorem exercise_2_1_13 (H : AddSubgroup ℚ) {x : ℚ}
  (hH : x ∈ H → (1 / x) ∈ H):
  H = ⊥ ∨ H = ⊤ :=
sorry

theorem exercise_2_4_4 {G : Type*} [Group G] (H : Subgroup G) :
  closure ((H : Set G) \ {1}) = ⊤ :=
sorry

theorem exercise_2_4_16a {G : Type*} [Group G] {H : Subgroup G}
  (hH : H ≠ ⊤) :
  ∃ M : Subgroup G, M ≠ ⊤ ∧
  ∀ K : Subgroup G, M ≤ K → K = M ∨ K = ⊤ ∧
  H ≤ M :=
sorry

theorem exercise_2_4_16b {n : ℕ} {hn : n ≠ 0}
  {R : Subgroup (DihedralGroup n)}
  (hR : R = Subgroup.closure {DihedralGroup.r 1}) :
  R ≠ ⊤ ∧
  ∀ K : Subgroup (DihedralGroup n), R ≤ K → K = R ∨ K = ⊤ :=
sorry

theorem exercise_2_4_16c {n : ℕ} (H : AddSubgroup (ZMod n)) :
  ∃ p : (ZMod n), Prime p ∧ H = AddSubgroup.closure {p} ↔
  (H ≠ ⊤ ∧ ∀ K : AddSubgroup (ZMod n), H ≤ K → K = H ∨ K = ⊤) :=
sorry

theorem exercise_3_1_3a {A : Type*} [CommGroup A] (B : Subgroup A) :
  ∀ a b : A ⧸ B, a*b = b*a :=
sorry

theorem exercise_3_1_22a (G : Type*) [Group G] (H K : Subgroup G)
  [Normal H] [Normal K] :
  Normal (H ⊓ K) :=
sorry

theorem exercise_3_1_22b {G : Type*} [Group G] (I : Type*)
  (H : I → Subgroup G) (hH : ∀ i : I, Normal (H i)) :
  Normal (⨅ (i : I), H i):=
sorry

theorem exercise_3_2_8 {G : Type*} [Group G] (H K : Subgroup G)
  [Fintype H] [Fintype K]
  (hHK : Nat.Coprime (card H) (card K)) :
  H ⊓ K = ⊥ :=
sorry

theorem exercise_3_2_11 {G : Type*} [Group G] {H K : Subgroup G}
  (hHK : H ≤ K) :
  H.index = K.index * H.relindex K :=
sorry

theorem exercise_3_2_16 (p : ℕ) (hp : Nat.Prime p) (a : ℕ) :
  Nat.Coprime a p → a ^ p ≡ a [ZMOD p] :=
sorry

theorem exercise_3_2_21a (H : AddSubgroup ℚ) (hH : H ≠ ⊤) : H.index = 0 :=
sorry

theorem exercise_3_3_3 {p : Nat.Primes} {G : Type*} [Group G]
  {H : Subgroup G} [hH : H.Normal] (hH1 : H.index = p) :
  ∀ K : Subgroup G, K ≤ H ∨ H ⊔ K = ⊤ ∨ (K ⊓ H).relindex K = p :=
sorry

theorem exercise_3_4_1 (G : Type*) [CommGroup G] [IsSimpleGroup G] :
    IsCyclic G ∧ ∃ G_fin : Fintype G, Nat.Prime (@card G G_fin) :=
sorry

theorem exercise_3_4_4 {G : Type*} [CommGroup G] [Fintype G] {n : ℕ}
    (hn : n ∣ (card G)) :
    ∃ (H : Subgroup G) (H_fin : Fintype H), @card H H_fin = n  :=
sorry

theorem exercise_3_4_5a {G : Type*} [Group G]
  (H : Subgroup G) [IsSolvable G] : IsSolvable H :=
sorry

theorem exercise_3_4_5b {G : Type*} [Group G] [IsSolvable G]
  (H : Subgroup G) [Normal H] :
  IsSolvable (G ⧸ H) :=
sorry

theorem exercise_3_4_11 {G : Type*} [Group G] [IsSolvable G]
  {H : Subgroup G} (hH : H ≠ ⊥) [H.Normal] :
  ∃ A ≤ H, A.Normal ∧ ∀ a b : A, a*b = b*a :=
sorry

theorem exercise_4_2_8 {G : Type*} [Group G] {H : Subgroup G}
  {n : ℕ} (hn : n > 0) (hH : H.index = n) :
  ∃ K ≤ H, K.Normal ∧ K.index ≤ n.factorial :=
sorry

theorem exercise_4_3_26 {α : Type*} [Fintype α] (ha : card α > 1)
  (h_tran : ∀ a b: α, ∃ σ : Equiv.Perm α, σ a = b) :
  ∃ σ : Equiv.Perm α, ∀ a : α, σ a ≠ a :=
sorry

theorem exercise_4_2_9a {G : Type*} [Fintype G] [Group G] {p α : ℕ}
  (hp : p.Prime) (ha : α > 0) (hG : card G = p ^ α) :
  ∀ H : Subgroup G, H.index = p → H.Normal :=
sorry

theorem exercise_4_2_14 {G : Type*} [Fintype G] [Group G]
  (hG : ¬ (card G).Prime) (hG1 : ∀ k : ℕ, k ∣ card G →
  ∃ (H : Subgroup G) (fH : Fintype H), @card H fH = k) :
  ¬ IsSimpleGroup G :=
sorry

theorem exercise_4_4_2 {G : Type*} [Fintype G] [Group G]
  {p q : Nat.Primes} (hpq : p ≠ q) (hG : card G = p*q) :
  IsCyclic G :=
sorry

theorem exercise_4_4_6a {G : Type*} [Group G] (H : Subgroup G)
  [Characteristic H] : Normal H  :=
sorry

theorem exercise_4_4_6b :
  ∃ (G : Type*) (hG : Group G) (H : @Subgroup G hG), @Characteristic G hG H  ∧ ¬ @Normal G hG H :=
sorry

theorem exercise_4_4_7 {G : Type*} [Group G] {H : Subgroup G} [Fintype H]
  (hH : ∀ (K : Subgroup G) (fK : Fintype K), card H = @card K fK → H = K) :
  H.Characteristic :=
sorry

theorem exercise_4_4_8a {G : Type*} [Group G] (H K : Subgroup G)
  (hHK : H ≤ K) [hHK1 : (H.subgroupOf K).Normal] [hK : K.Normal] :
  H.Normal :=
sorry

theorem exercise_4_5_1a {p : ℕ} {G : Type*} [Group G]
  {P : Subgroup G} (hP : IsPGroup p P) (H : Subgroup G)
  (hH : P ≤ H) : IsPGroup p H :=
sorry

theorem exercise_4_5_13 {G : Type*} [Group G] [Fintype G]
  (hG : card G = 56) :
  ∃ (p : ℕ) (P : Sylow p G), P.Normal :=
sorry

theorem exercise_4_5_14 {G : Type*} [Group G] [Fintype G]
  (hG : card G = 312) :
  ∃ (p : ℕ) (P : Sylow p G), P.Normal :=
sorry

theorem exercise_4_5_15 {G : Type*} [Group G] [Fintype G]
  (hG : card G = 351) :
  ∃ (p : ℕ) (P : Sylow p G), P.Normal :=
sorry

theorem exercise_4_5_16 {p q r : ℕ} {G : Type*} [Group G]
  [Fintype G]  (hpqr : p < q ∧ q < r)
  (hpqr1 : p.Prime ∧ q.Prime ∧ r.Prime)(hG : card G = p*q*r) :
  Nonempty (Sylow p G) ∨ Nonempty (Sylow q G) ∨ Nonempty (Sylow r G) :=
sorry

theorem exercise_4_5_17 {G : Type*} [Fintype G] [Group G]
  (hG : card G = 105) :
  Nonempty (Sylow 5 G) ∧ Nonempty (Sylow 7 G) :=
sorry

theorem exercise_4_5_18 {G : Type*} [Fintype G] [Group G]
  (hG : card G = 200) :
  ∃ N : Sylow 5 G, N.Normal :=
sorry

theorem exercise_4_5_19 {G : Type*} [Fintype G] [Group G]
  (hG : card G = 6545) : ¬ IsSimpleGroup G :=
sorry

theorem exercise_4_5_20 {G : Type*} [Fintype G] [Group G]
  (hG : card G = 1365) : ¬ IsSimpleGroup G :=
sorry

theorem exercise_4_5_21 {G : Type*} [Fintype G] [Group G]
  (hG : card G = 2907) : ¬ IsSimpleGroup G :=
sorry

theorem exercise_4_5_22 {G : Type*} [Fintype G] [Group G]
  (hG : card G = 132) : ¬ IsSimpleGroup G :=
sorry

theorem exercise_4_5_23 {G : Type*} [Fintype G] [Group G]
  (hG : card G = 462) : ¬ IsSimpleGroup G :=
sorry

theorem exercise_4_5_28 {G : Type*} [Group G] [Fintype G]
  (hG : card G = 105) (P : Sylow 3 G) [hP : P.Normal] :
  CommGroup G :=
sorry

theorem exercise_4_5_33 {G : Type*} [Group G] [Fintype G] {p : ℕ}
  (P : Sylow p G) [hP : P.Normal] (H : Subgroup G) [Fintype H] :
  ∀ R : Sylow p H, R.toSubgroup = (H ⊓ P.toSubgroup).subgroupOf H ∧
  Nonempty (Sylow p H) :=
sorry

theorem exercise_5_4_2 {G : Type*} [Group G] (H : Subgroup G) :
  H.Normal ↔ ⁅(⊤ : Subgroup G), H⁆ ≤ H :=
sorry

theorem exercise_7_1_2 {R : Type*} [Ring R] {u : R}
  (hu : IsUnit u) : IsUnit (-u) :=
sorry

theorem exercise_7_1_11 {R : Type*} [Ring R] [IsDomain R]
  {x : R} (hx : x^2 = 1) : x = 1 ∨ x = -1 :=
sorry

theorem exercise_7_1_12 {F : Type*} [Field F] {K : Subring F}
  (hK : (1 : F) ∈ K) : IsDomain K :=
sorry

theorem exercise_7_1_15 {R : Type*} [Ring R] (hR : ∀ a : R, a^2 = a) :
  CommRing R :=
sorry

theorem exercise_7_2_2 {R : Type*} [Ring R] (p : Polynomial R) :
  p ∣ 0 ↔ ∃ b : R, b ≠ 0 ∧ b • p = 0 :=
sorry

theorem exercise_7_2_12 {R G : Type*} [Ring R] [Group G] [Fintype G] :
  ∑ g : G, MonoidAlgebra.of R G g ∈ center (MonoidAlgebra R G) :=
sorry

theorem exercise_7_3_16 {R S : Type*} [Ring R] [Ring S]
  {φ : R →+* S} (hf : Function.Surjective φ) :
  φ '' (center R) ⊂ center S :=
sorry

theorem exercise_7_3_37 {p m : ℕ} (hp : p.Prime) :
  IsNilpotent (span ({↑p} : Set $ ZMod $ p^m) : Ideal $ ZMod $ p^m) :=
sorry

theorem exercise_7_4_27 {R : Type*} [CommRing R] (hR : (0 : R) ≠ 1)
  {a : R} (ha : IsNilpotent a) (b : R) :
  IsUnit (1-a*b) :=
sorry

theorem exercise_8_1_12 {N : ℕ} (hN : N > 0) {M M': ℤ} {d : ℕ}
  (hMN : M.gcd N = 1) (hMd : d.gcd N.totient = 1)
  (hM' : M' ≡ M^d [ZMOD N]) :
  ∃ d' : ℕ, d' * d ≡ 1 [ZMOD N.totient] ∧
  M ≡ M'^d' [ZMOD N] :=
sorry

theorem exercise_8_2_4 {R : Type*} [Ring R][NoZeroDivisors R]
  [CancelCommMonoidWithZero R] [GCDMonoid R]
  (h1 : ∀ a b : R, a ≠ 0 → b ≠ 0 → ∃ r s : R, gcd a b = r*a + s*b)
  (h2 : ∀ a : ℕ → R, (∀ i j : ℕ, i < j → a i ∣ a j) →
  ∃ N : ℕ, ∀ n ≥ N, ∃ u : R, IsUnit u ∧ a n = u * a N) :
  IsPrincipalIdealRing R :=
sorry

theorem exercise_8_3_4 {R : Type*} {n : ℤ} {r s : ℚ}
  (h : r^2 + s^2 = n) :
  ∃ a b : ℤ, a^2 + b^2 = n :=
sorry

theorem exercise_8_3_5a {n : ℤ} (hn0 : n > 3) (hn1 : Squarefree n) :
  Irreducible (2 : Zsqrtd $ -n) ∧
  Irreducible (⟨0, 1⟩ : Zsqrtd $ -n) ∧
  Irreducible (1 + ⟨0, 1⟩ : Zsqrtd $ -n) :=
sorry

theorem exercise_8_3_6a {R : Type} [Ring R]
  (hR : R = (GaussianInt ⧸ span ({⟨0, 1⟩} : Set GaussianInt))) :
  IsField R ∧ ∃ finR : Fintype R, @card R finR = 2 :=
sorry

theorem exercise_8_3_6b {q : ℕ} (hq0 : q.Prime)
  (hq1 : q ≡ 3 [ZMOD 4]) {R : Type} [Ring R]
  (hR : R = (GaussianInt ⧸ span ({↑q} : Set GaussianInt))) :
  IsField R ∧ ∃ finR : Fintype R, @card R finR = q^2 :=
sorry

theorem exercise_9_1_6 : ¬ Submodule.IsPrincipal
  (span ({MvPolynomial.X 0, MvPolynomial.X 1} : Set (MvPolynomial (Fin 2) ℚ))) :=
sorry

theorem exercise_9_1_10 {f : ℕ → MvPolynomial ℕ ℤ}
  (hf : f = λ i => MvPolynomial.X i * MvPolynomial.X (i+1)):
  Infinite (minimalPrimes (MvPolynomial ℕ ℤ ⧸ span (range f))) :=
sorry

theorem exercise_9_3_2 {f g : Polynomial ℚ} (i j : ℕ)
  (hfg : ∀ n : ℕ, ∃ a : ℤ, (f*g).coeff = a) :
  ∃ a : ℤ, f.coeff i * g.coeff j = a :=
sorry

theorem exercise_9_4_2a : Irreducible (X^4 - 4*X^3 + 6 : Polynomial ℤ) :=
sorry

theorem exercise_9_4_2b : Irreducible
  (X^6 + 30*X^5 - 15*X^3 + 6*X - 120 : Polynomial ℤ) :=
sorry

theorem exercise_9_4_2c : Irreducible
  (X^4 + 4*X^3 + 6*X^2 + 2*X + 1 : Polynomial ℤ) :=
sorry

theorem exercise_9_4_2d {p : ℕ} (hp : p.Prime ∧ p > 2)
  {f : Polynomial ℤ} (hf : f = (X + 2)^p):
  Irreducible (∑ n in (f.support \ {0}), (f.coeff n : Polynomial ℤ) * X ^ (n-1) :
  Polynomial ℤ) :=
sorry

theorem exercise_9_4_9 :
  Irreducible (X^2 - C Zsqrtd.sqrtd : Polynomial (Zsqrtd 2)) :=
sorry

theorem exercise_9_4_11 :
  Irreducible ((MvPolynomial.X 0)^2 + (MvPolynomial.X 1)^2 - 1 : MvPolynomial (Fin 2) ℚ) :=
sorry

theorem exercise_11_1_13 {ι : Type*} [Fintype ι] :
  (ι → ℝ) ≃ₗ[ℚ] ℝ :=
sorry
