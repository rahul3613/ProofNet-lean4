import Mathlib

open scoped BigOperators

theorem exercise_2020_b5 (z : Fin 4 → ℂ) (hz0 : ∀ n, ‖z n‖ < 1)
  (hz1 : ∀ n : Fin 4, z n ≠ 1) :
  3 - z 0 - z 1 - z 2 - z 3 + (z 0) * (z 1) * (z 2) * (z 3) ≠ 0 :=
sorry

theorem exercise_2018_a5 (f : ℝ → ℝ) (hf : ContDiff ℝ ⊤ f)
  (hf0 : f 0 = 0) (hf1 : f 1 = 1) (hf2 : ∀ x, f x ≥ 0) :
  ∃ (n : ℕ) (x : ℝ), iteratedDeriv n f x = 0 :=
sorry

theorem exercise_2018_b2 (n : ℕ) (hn : n > 0) (f : ℕ → ℂ → ℂ)
  (hf : ∀ n : ℕ, f n = λ (z : ℂ) => (∑ i : Fin n, (n-i)* z^(i : ℕ))) :
  ¬ (∃ z : ℂ, ‖z‖ ≤ 1 ∧ f n z = 0) :=
sorry

theorem exercise_2018_b4 (a : ℝ) (x : ℕ → ℝ) (hx0 : x 0 = a)
  (hx1 : x 1 = a)
  (hxn : ∀ n : ℕ, n ≥ 2 → x (n+1) = 2*(x n)*(x (n-1)) - x (n-2))
  (h : ∃ n, x n = 0) :
  ∃ c, Function.Periodic x c :=
sorry

theorem exercise_2017_b3 (f : ℝ → ℝ) (c : ℕ → ℝ)
  (hf : f = λ x => (∑' (i : ℕ), (c i) * x^i))
  (hc : ∀ n, c n = 0 ∨ c n = 1)
  (hf1 : f (2/3) = 3/2) :
  Irrational (f (1/2)) :=
sorry

theorem exercise_2014_a5 (P : ℕ → Polynomial ℤ)
  (hP : ∀ n, P n = ∑ i : Fin n, (n+1) * Polynomial.X ^ n) :
  ∀ (j k : ℕ), j ≠ k → IsCoprime (P j) (P k) :=
sorry

theorem exercise_2010_a4 (n : ℕ) :
  ¬ Nat.Prime (10^10^10^n + 10^10^n + 10^n - 1) :=
sorry

theorem exercise_2001_a5 :
  ∃! a n : ℕ, a > 0 ∧ n > 0 ∧ a^(n+1) - (a+1)^n = 2001 :=
sorry

theorem exercise_2000_a2 :
  ∀ N : ℕ, ∃ n : ℕ, n > N ∧ ∃ i : Fin 6 → ℕ, n = (i 0)^2 + (i 1)^2 ∧
  n + 1 = (i 2)^2 + (i 3)^2 ∧ n + 2 = (i 4)^2 + (i 5)^2 :=
sorry

theorem exercise_1999_b4 (f : ℝ → ℝ) (hf: ContDiff ℝ 3 f)
  (hf1 : ∀ n ≤ 3, ∀ x : ℝ, iteratedDeriv n f x > 0)
  (hf2 : ∀ x : ℝ, iteratedDeriv 3 f x ≤ f x) :
  ∀ x : ℝ, deriv f x < 2 * f x :=
sorry

theorem exercise_1998_a3 (f : ℝ → ℝ) (hf : ContDiff ℝ 3 f) :
  ∃ a : ℝ, (f a) * (deriv f a) * (iteratedDeriv 2 f a) * (iteratedDeriv 3 f a) ≥ 0 :=
sorry

theorem exercise_1998_b6 (a b c : ℤ) :
  ∃ n : ℤ, n > 0 ∧ ¬ ∃ m : ℤ, Real.sqrt (n^3 + a*n^2 + b*n + c) = m :=
sorry
