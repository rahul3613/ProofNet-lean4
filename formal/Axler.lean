import Mathlib

open Fintype Complex Polynomial LinearMap FiniteDimensional Module Module.End
open scoped BigOperators

theorem exercise_1_2 :
  (⟨-1/2, Real.sqrt 3 / 2⟩ : ℂ) ^ 3 = -1 :=
sorry

theorem exercise_1_3 {F V : Type*} [AddCommGroup V] [Field F]
  [Module F V] {v : V} : -(-v) = v :=
sorry

theorem exercise_1_4 {F V : Type*} [AddCommGroup V] [Field F]
  [Module F V] (v : V) (a : F): a • v = 0 ↔ a = 0 ∨ v = 0 :=
sorry

theorem exercise_1_6 : ∃ U : Set (ℝ × ℝ),
  (U ≠ ∅) ∧
  (∀ (u v : ℝ × ℝ), u ∈ U ∧ v ∈ U → u + v ∈ U) ∧
  (∀ (u : ℝ × ℝ), u ∈ U → -u ∈ U) ∧
  (∀ U' : Submodule ℝ (ℝ × ℝ), U ≠ ↑U') :=
sorry

theorem exercise_1_7 : ∃ U : Set (ℝ × ℝ),
  (U ≠ ∅) ∧
  (∀ (c : ℝ) (u : ℝ × ℝ), u ∈ U → c • u ∈ U) ∧
  (∀ U' : Submodule ℝ (ℝ × ℝ), U ≠ ↑U') :=
sorry

theorem exercise_1_8 {F V : Type*} [AddCommGroup V] [Field F]
  [Module F V] {ι : Type*} (u : ι → Submodule F V) :
  ∃ U : Submodule F V, (⋂ (i : ι), (u i).carrier) = ↑U :=
  by
sorry

theorem exercise_1_9 {F V : Type*} [AddCommGroup V] [Field F]
  [Module F V] (U W : Submodule F V):
  ∃ U' : Submodule F V, (U'.carrier = ↑U ∩ ↑W ↔ (U ≤ W ∨ W ≤ U)) :=
sorry

theorem exercise_3_1 {F V : Type*}
  [AddCommGroup V] [Field F] [Module F V] [FiniteDimensional F V]
  (T : V →ₗ[F] V) (hT : finrank F V = 1) :
  ∃ c : F, ∀ v : V, T v = c • v:=
sorry

theorem exercise_3_8 {F V W : Type*}  [AddCommGroup V]
  [AddCommGroup W] [Field F] [Module F V] [Module F W]
  (L : V →ₗ[F] W) :
  ∃ U : Submodule F V, U ⊓ (ker L) = ⊥ ∧
  (range L = range (domRestrict L U)):=
sorry

theorem exercise_4_4 (p : Polynomial ℂ) :
  p.degree = @card (rootSet p ℂ) (rootSetFintype p ℂ) ↔
  Disjoint
  (@card (rootSet (derivative p) ℂ) (rootSetFintype (derivative p) ℂ))
  (@card (rootSet p ℂ) (rootSetFintype p ℂ)) :=
sorry

theorem exercise_5_1 {F V : Type*} [AddCommGroup V] [Field F]
  [Module F V] {L : V →ₗ[F] V} {n : ℕ} (U : Fin n → Submodule F V)
  (hU : ∀ i : Fin n, Submodule.map L (U i) = U i) :
  Submodule.map L (∑ i : Fin n, U i : Submodule F V) =
  (∑ i : Fin n, U i : Submodule F V) :=
sorry

theorem exercise_5_4 {F V : Type*} [AddCommGroup V] [Field F]
  [Module F V] (S T : V →ₗ[F] V) (hST : S ∘ T = T ∘ S) (c : F):
  Submodule.map S (ker (T - c • LinearMap.id)) = ker (T - c • LinearMap.id) :=
sorry

theorem exercise_5_11 {F V : Type*} [AddCommGroup V] [Field F]
  [Module F V] (S T : End F V) :
  (S * T).Eigenvalues = (T * S).Eigenvalues :=
sorry

theorem exercise_5_12 {F V : Type*} [AddCommGroup V] [Field F]
  [Module F V] {S : End F V}
  (hS : ∀ v : V, ∃ c : F, v ∈ eigenspace S c) :
  ∃ c : F, S = c • LinearMap.id :=
sorry

theorem exercise_5_13 {F V : Type*} [AddCommGroup V] [Field F]
  [Module F V] [FiniteDimensional F V] {T : End F V}
  (hS : ∀ U : Submodule F V, finrank F U = finrank F V - 1 →
  Submodule.map T U = U) : ∃ c : F, T = c • LinearMap.id :=
sorry

theorem exercise_5_20 {F V : Type*} [AddCommGroup V] [Field F]
  [Module F V] [FiniteDimensional F V] {S T : End F V}
  (h1 : card (T.Eigenvalues) = finrank F V)
  (h2 : ∀ v : V, ∃ c : F, v ∈ eigenspace S c ↔ ∃ c : F, v ∈ eigenspace T c) :
  S * T = T * S :=
sorry

theorem exercise_5_24 {V : Type*} [AddCommGroup V]
  [Module ℝ V] [FiniteDimensional ℝ V] {T : End ℝ V}
  (hT : ∀ c : ℝ, eigenspace T c = ⊥) {U : Submodule ℝ V}
  (hU : Submodule.map T U = U) : Even (finrank U) :=
sorry

theorem exercise_6_2 {V : Type*} [NormedAddCommGroup V] [Module ℂ V]
[InnerProductSpace ℂ V] (u v : V) :
  ⟪u, v⟫_ℂ = 0 ↔ ∀ (a : ℂ), ‖u‖  ≤ ‖u + a • v‖ :=
sorry

theorem exercise_6_3 {n : ℕ} (a b : Fin n → ℝ) :
  (∑ i, a i * b i) ^ 2 ≤ (∑ i : Fin n, i * a i ^ 2) * (∑ i, b i ^ 2 / i) :=
sorry

theorem exercise_6_7 {V : Type*} [NormedAddCommGroup V] [InnerProductSpace ℂ V] (u v : V) :
  ⟪u, v⟫_ℂ = (‖u + v‖^2 - ‖u - v‖^2 + I*‖u + I•v‖^2 - I*‖u-I•v‖^2) / 4 :=
sorry

theorem exercise_6_13 {V : Type*} [NormedAddCommGroup V] [InnerProductSpace ℂ V] {n : ℕ}
  {e : Fin n → V} (he : Orthonormal ℂ e) (v : V) :
  ‖v‖^2 = ∑ i : Fin n, ‖⟪v, e i⟫_ℂ‖^2 ↔ v ∈ Submodule.span ℂ (e '' Set.univ) :=
sorry

theorem exercise_6_16 {K V : Type*} [IsROrC K] [NormedAddCommGroup V] [InnerProductSpace K V]
  {U : Submodule K V} :
  U.orthogonal = ⊥  ↔ U = ⊤ :=
sorry

theorem exercise_7_5 {V : Type*} [NormedAddCommGroup V] [InnerProductSpace ℂ V]
  [FiniteDimensional ℂ V] (hV : finrank V ≥ 2) :
  ∀ U : Submodule ℂ (End ℂ V), U.carrier ≠
  {T | T * adjoint T = adjoint T * T} :=
sorry

theorem exercise_7_6 {V : Type*} [NormedAddCommGroup V] [InnerProductSpace ℂ V]
  [FiniteDimensional ℂ V] (T : End ℂ V)
  (hT : T * adjoint T = adjoint T * T) :
  range T = range (adjoint T) :=
sorry

theorem exercise_7_9 {V : Type*} [NormedAddCommGroup V] [InnerProductSpace ℂ V]
  [FiniteDimensional ℂ V] (T : End ℂ V)
  (hT : T * adjoint T = adjoint T * T) :
  IsSelfAdjoint T ↔ ∀ e : T.Eigenvalues, (e : ℂ).im = 0 :=
sorry

theorem exercise_7_10 {V : Type*} [NormedAddCommGroup V] [InnerProductSpace ℂ V]
  [FiniteDimensional ℂ V] (T : End ℂ V)
  (hT : T * adjoint T = adjoint T * T) (hT1 : T^9 = T^8) :
  IsSelfAdjoint T ∧ T^2 = T :=
sorry

theorem exercise_7_11 {V : Type*} [NormedAddCommGroup V] [InnerProductSpace ℂ V]
  [FiniteDimensional ℂ V] {T : End ℂ V} (hT : T*adjoint T = adjoint T*T) :
  ∃ (S : End ℂ V), S ^ 2 = T :=
sorry

theorem exercise_7_14 {𝕜 V : Type*} [IsROrC 𝕜] [NormedAddCommGroup V]
  [InnerProductSpace 𝕜 V] [FiniteDimensional 𝕜 V]
  {T : End 𝕜 V} (hT : IsSelfAdjoint T)
  {l : 𝕜} {ε : ℝ} (he : ε > 0) : ∃ v : V, ‖v‖= 1 ∧ (‖T v - l • v‖ < ε →
  (∃ l' : T.Eigenvalues, ‖l - l'‖ < ε)) :=
sorry
