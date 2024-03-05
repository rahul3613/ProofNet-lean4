import Mathlib

open Filter Set

theorem exercise_2022_IA_4_I_1E_a : ∀ N : ℕ, ∃ n ≥ N, (3*n+1).Prime ∧ (3*n+1) ≥ N :=
sorry

theorem exercise_2022_IA_4_I_2D_a : Irrational (2^((1:ℝ)/3) + 3^((1:ℝ)/3)) :=
sorry

theorem exercise_2022_IB_3_II_13G_a_i (U : Set ℂ) (hU : IsOpen U)
  (hU1 : Nonempty U) (hU2 : IsConnected U) (f : ℕ → ℂ → ℂ) (f' : ℂ → ℂ)
  (hf : ∀ n : ℕ, DifferentiableOn ℂ (f n) U)
  (hf1 : ∀ X ⊂ U, CompactSpace X →
  (TendstoUniformly (λ n => restrict X (f n)) (restrict X f') atTop)) :
  DifferentiableOn ℂ f' U :=
sorry
