import Mathlib

open Filter Set TopologicalSpace
open scoped Topology
noncomputable section

theorem exercise_13_1 (X : Type*) [TopologicalSpace X] (A : Set X)
  (h1 : ∀ x ∈ A, ∃ U : Set X, x ∈ U ∧ IsOpen U ∧ U ⊆ A) :
  IsOpen A :=
sorry

theorem exercise_13_3b : ¬ ∀ X : Type, ∀s : Set (Set X),
  (∀ t : Set X, t ∈ s → (Set.Infinite tᶜ ∨ t = ∅ ∨ t = ⊤)) →
  (Set.Infinite (⋃₀ s)ᶜ ∨ (⋃₀ s) = ∅ ∨ (⋃₀ s) = ⊤) :=
sorry

def is_topology (X : Type*) (T : Set (Set X)) :=
  univ ∈ T ∧
  (∀ s t, s ∈ T → t ∈ T → s ∩ t ∈ T) ∧
  (∀s, (∀t ∈ s, t ∈ T) → sUnion s ∈ T)

theorem exercise_13_4a1 (X I : Type*) (T : I → Set (Set X)) (h : ∀ i, is_topology X (T i)) :
  is_topology X (⋂ i : I, T i) :=
sorry

theorem exercise_13_4a2 :
  ∃ (X I : Type*) (T : I → Set (Set X)),
  (∀ i, is_topology X (T i)) ∧ ¬  is_topology X (⋂ i : I, T i) :=
sorry

theorem exercise_13_4b1 (X I : Type*) (T : I → Set (Set X)) (h : ∀ i, is_topology X (T i)) :
  ∃! T', is_topology X T' ∧ (∀ i, T i ⊆ T') ∧
  ∀ T'', is_topology X T'' → (∀ i, T i ⊆ T'') → T'' ⊆ T' :=
sorry

theorem exercise_13_4b2 (X I : Type*) (T : I → Set (Set X)) (h : ∀ i, is_topology X (T i)) :
  ∃! T', is_topology X T' ∧ (∀ i, T' ⊆ T i) ∧
  ∀ T'', is_topology X T'' → (∀ i, T'' ⊆ T i) → T' ⊆ T'' :=
sorry

theorem exercise_13_5a {X : Type*}
  [TopologicalSpace X] (A : Set (Set X)) (hA : IsTopologicalBasis A) :
  generateFrom A = generateFrom (sInter {T | is_topology X T ∧ A ⊆ T}) :=
sorry

theorem exercise_13_5b {X : Type*}
  [t : TopologicalSpace X] (A : Set (Set X)) (hA : t = generateFrom A) :
  generateFrom A = generateFrom (sInter {T | is_topology X T ∧ A ⊆ T}) :=
sorry

def lower_limit_topology (X : Type) [Preorder X] :=
  generateFrom {S : Set X | ∃ a b, a < b ∧ S = Ico a b}

def Rl := lower_limit_topology ℝ

def K : Set ℝ := {r | ∃ n : ℕ, r = 1 / n}

def K_topology := generateFrom
  ({S : Set ℝ | ∃ a b, a < b ∧ S = Ioo a b} ∪ {S : Set ℝ | ∃ a b, a < b ∧ S = Ioo a b \ K})

theorem exercise_13_6 :
  ¬ (∀ U, Rl.IsOpen U → K_topology.IsOpen U) ∧ ¬ (∀ U, K_topology.IsOpen U → Rl.IsOpen U) :=
sorry

theorem exercise_13_8a :
  IsTopologicalBasis {S : Set ℝ | ∃ a b : ℚ, a < b ∧ S = Ioo ↑a ↑b} :=
sorry

theorem exercise_13_8b :
  (generateFrom {S : Set ℝ | ∃ a b : ℚ, a < b ∧ S = Ico ↑a ↑b}).IsOpen ≠
  (lower_limit_topology ℝ).IsOpen :=
sorry

theorem exercise_16_1 {X : Type*} [TopologicalSpace X]
  (Y : Set X)
  (A : Set Y) :
  ∀ U : Set A, IsOpen U ↔ IsOpen (Subtype.val '' U) :=
sorry

theorem exercise_16_4 {X Y : Type*} [TopologicalSpace X] [TopologicalSpace Y]
  (π₁ : X × Y → X)
  (π₂ : X × Y → Y)
  (h₁ : π₁ = Prod.fst)
  (h₂ : π₂ = Prod.snd) :
  IsOpenMap π₁ ∧ IsOpenMap π₂ :=
sorry

def rational (x : ℝ) := x ∈ range ((↑) : ℚ → ℝ)

theorem exercise_16_6
  (S : Set (Set (ℝ × ℝ)))
  (hS : ∀ s, s ∈ S → ∃ a b c d, (rational a ∧ rational b ∧ rational c ∧ rational d
  ∧ s = {x | ∃ x₁ x₂, x = (x₁, x₂) ∧ a < x₁ ∧ x₁ < b ∧ c < x₂ ∧ x₂ < d})) :
  IsTopologicalBasis S :=
sorry

theorem exercise_17_4 {X : Type*} [TopologicalSpace X]
  (U A : Set X) (hU : IsOpen U) (hA : IsClosed A) :
  IsOpen (U \ A) ∧ IsClosed (A \ U) :=
sorry

theorem exercise_18_8a {X Y : Type*} [TopologicalSpace X] [TopologicalSpace Y]
  [LinearOrder Y] [OrderTopology Y] {f g : X → Y}
  (hf : Continuous f) (hg : Continuous g) :
  IsClosed {x | f x ≤ g x} :=
sorry

theorem exercise_18_8b {X Y : Type*} [TopologicalSpace X] [TopologicalSpace Y]
  [LinearOrder Y] [OrderTopology Y] {f g : X → Y}
  (hf : Continuous f) (hg : Continuous g) :
  Continuous (λ x => min (f x) (g x)) :=
sorry

theorem exercise_18_13
  {X : Type*} [TopologicalSpace X] {Y : Type*} [TopologicalSpace Y]
  [T2Space Y] {A : Set X} {f : A → Y} (hf : Continuous f)
  (g : closure A → Y)
  (g_con : Continuous g) :
  ∀ (g' : closure A → Y), Continuous g' →  (∀ (x : closure A), g x = g' x) :=
sorry

theorem exercise_19_6a
  {n : ℕ}
  {f : Fin n → Type*} {x : ℕ → Πa, f a}
  (y : Πi, f i)
  [Πa, TopologicalSpace (f a)] :
  Tendsto x atTop (𝓝 y) ↔ ∀ i, Tendsto (λ j => (x j) i) atTop (𝓝 (y i)) :=
sorry

theorem exercise_20_2
  [TopologicalSpace (ℝ ×ₗ ℝ)] [OrderTopology (ℝ ×ₗ ℝ)]
  : MetrizableSpace (ℝ ×ₗ ℝ) :=
sorry

abbrev I : Set ℝ := Icc 0 1

theorem exercise_21_6a
  (f : ℕ → I → ℝ )
  (h : ∀ x n, f n x = x ^ n) :
  ∀ x, ∃ y, Tendsto (λ n => f n x) atTop (𝓝 y) :=
sorry

theorem exercise_21_6b
  (f : ℕ → I → ℝ )
  (h : ∀ x n, f n x = x ^ n) :
  ¬ ∃ f₀, TendstoUniformly f f₀ atTop :=
sorry

theorem exercise_21_8
  {X : Type*} [TopologicalSpace X] {Y : Type*} [MetricSpace Y]
  {f : ℕ → X → Y} {x : ℕ → X}
  (hf : ∀ n, Continuous (f n))
  (x₀ : X)
  (hx : Tendsto x atTop (𝓝 x₀))
  (f₀ : X → Y)
  (hh : TendstoUniformly f f₀ atTop) :
  Tendsto (λ n => f n (x n)) atTop (𝓝 (f₀ x₀)) :=
sorry

theorem exercise_22_2a {X Y : Type*} [TopologicalSpace X]
  [TopologicalSpace Y] (p : X → Y) (h : Continuous p) :
  QuotientMap p ↔ ∃ (f : Y → X), Continuous f ∧ p ∘ f = id :=
sorry

theorem exercise_22_2b {X : Type*} [TopologicalSpace X]
  {A : Set X} (r : X → A) (hr : Continuous r) (h : ∀ x : A, r x = x) :
  QuotientMap r :=
sorry

theorem exercise_22_5 {X Y : Type*} [TopologicalSpace X]
  [TopologicalSpace Y] (p : X → Y) (hp : IsOpenMap p)
  (A : Set X) (hA : IsOpen A) : IsOpenMap (p ∘ Subtype.val : A → Y) :=
sorry

theorem exercise_23_2 {X : Type*}
  [TopologicalSpace X] {A : ℕ → Set X} (hA : ∀ n, IsConnected (A n))
  (hAn : ∀ n, A n ∩ A (n + 1) ≠ ∅) :
  IsConnected (⋃ n, A n) :=
sorry

theorem exercise_23_3 {X : Type*} [TopologicalSpace X]
  [TopologicalSpace X] {A : ℕ → Set X}
  (hAn : ∀ n, IsConnected (A n))
  (A₀ : Set X)
  (hA : IsConnected A₀)
  (h : ∀ n, A₀ ∩ A n ≠ ∅) :
  IsConnected (A₀ ∪ (⋃ n, A n)) :=
sorry

set_option checkBinderAnnotations false

theorem exercise_23_4 {X : Type*} [TopologicalSpace X] [CofiniteTopology X]
  (s : Set X) : Infinite s → IsConnected s :=
sorry

theorem exercise_23_6 {X : Type*}
  [TopologicalSpace X] {A C : Set X} (hc : IsConnected C)
  (hCA : C ∩ A ≠ ∅) (hCXA : C ∩ Aᶜ ≠ ∅) :
  C ∩ (frontier A) ≠ ∅ :=
sorry

theorem exercise_23_9 {X Y : Type*}
  [TopologicalSpace X] [TopologicalSpace Y]
  (A₁ A₂ : Set X)
  (B₁ B₂ : Set Y)
  (hA : A₁ ⊂ A₂)
  (hB : B₁ ⊂ B₂)
  (hA : IsConnected A₂)
  (hB : IsConnected B₂) :
  IsConnected ({x | ∃ a b, x = (a, b) ∧ a ∈ A₂ ∧ b ∈ B₂} \
      {x | ∃ a b, x = (a, b) ∧ a ∈ A₁ ∧ b ∈ B₁}) :=
sorry

theorem exercise_23_11 {X Y : Type*} [TopologicalSpace X] [TopologicalSpace Y]
  (p : X → Y) (hq : QuotientMap p)
  (hY : ConnectedSpace Y) (hX : ∀ y : Y, IsConnected (p ⁻¹' {y})) :
  ConnectedSpace X :=
sorry

theorem exercise_24_2 {f : (Metric.sphere 0 1 : Set ℝ) → ℝ}
  (hf : Continuous f) : ∃ x, f x = f (-x) :=
sorry

theorem exercise_24_3a [TopologicalSpace I] [CompactSpace I]
  (f : I → I) (hf : Continuous f) :
  ∃ (x : I), f x = x :=
sorry

theorem exercise_25_4 {X : Type*} [TopologicalSpace X]
  [LocPathConnectedSpace X] (U : Set X) (hU : IsOpen U)
  (hcU : IsConnected U) : IsPathConnected U :=
sorry

theorem exercise_25_9 {G : Type*} [TopologicalSpace G] [Group G]
  [TopologicalGroup G] (C : Set G) (h : C = connectedComponent 1) :
  IsNormalSubgroup C :=
sorry

theorem exercise_26_11
  {X : Type*} [TopologicalSpace X] [CompactSpace X] [T2Space X]
  (A : Set (Set X)) (hA : ∀ (a b : Set X), a ∈ A → b ∈ A → a ⊆ b ∨ b ⊆ a)
  (hA' : ∀ a ∈ A, IsClosed a) (hA'' : ∀ a ∈ A, IsConnected a) :
  IsConnected (⋂₀ A) :=
sorry

theorem exercise_26_12 {X Y : Type*} [TopologicalSpace X] [TopologicalSpace Y]
  (p : X → Y) (h : Function.Surjective p) (hc : Continuous p) (hp : ∀ y, IsCompact (p ⁻¹' {y}))
  (hY : CompactSpace Y) : CompactSpace X :=
sorry

theorem exercise_27_4
  {X : Type*} [MetricSpace X] [ConnectedSpace X] (hX : ∃ x y : X, x ≠ y) :
  ¬ Countable (univ : Set X) :=
sorry

def countably_compact (X : Type*) [TopologicalSpace X] :=
  ∀ U : ℕ → Set X,
  (∀ i, IsOpen (U i)) ∧ ((univ : Set X) ⊆ ⋃ i, U i) →
  (∃ t : Finset ℕ, (univ : Set X) ⊆ ⋃ i ∈ t, U i)

def limit_point_compact (X : Type*) [TopologicalSpace X] :=
  ∀ U : Set X, Infinite U → ∃ x ∈ U, ClusterPt x (𝓟 U)

theorem exercise_28_4 {X : Type*}
  [TopologicalSpace X] (hT1 : T1Space X) :
  countably_compact X ↔ limit_point_compact X :=
sorry

theorem exercise_28_5
  (X : Type*) [TopologicalSpace X] :
  countably_compact X ↔ ∀ (C : ℕ → Set X), (∀ n, IsClosed (C n)) ∧
  (∀ n, C n ≠ ∅) ∧ (∀ n, C n ⊆ C (n + 1)) → ∃ x, ∀ n, x ∈ C n :=
sorry

theorem exercise_28_6 {X : Type*} [MetricSpace X]
  [CompactSpace X] {f : X → X} (hf : Isometry f) :
  Function.Bijective f :=
sorry

theorem exercise_29_1 : ¬ LocallyCompactSpace ℚ :=
sorry

theorem exercise_29_4 [TopologicalSpace (ℕ → I)] :
  ¬ LocallyCompactSpace (ℕ → I) :=
sorry

theorem exercise_29_10 {X : Type*}
  [TopologicalSpace X] [T2Space X] (x : X)
  (hx : ∃ U : Set X, x ∈ U ∧ IsOpen U ∧ (∃ K : Set X, U ⊂ K ∧ IsCompact K))
  (U : Set X) (hU : IsOpen U) (hxU : x ∈ U) :
  ∃ (V : Set X), IsOpen V ∧ x ∈ V ∧ IsCompact (closure V) ∧ closure V ⊆ U :=
sorry

theorem exercise_30_10
  {X : ℕ → Type*} [∀ i, TopologicalSpace (X i)]
  (h : ∀ i, ∃ (s : Set (X i)), Countable s ∧ Dense s) :
  ∃ (s : Set (Π i, X i)), Countable s ∧ Dense s :=
sorry

theorem exercise_30_13 {X : Type*} [TopologicalSpace X]
  (h : ∃ (s : Set X), Countable s ∧ Dense s) (U : Set (Set X))
  (hU : ∀ (x y : Set X), x ∈ U → y ∈ U → x ≠ y → x ∩ y = ∅) :
  Countable U :=
sorry

theorem exercise_31_1 {X : Type*} [TopologicalSpace X]
  (hX : RegularSpace X) (x y : X) :
  ∃ (U V : Set X), IsOpen U ∧ IsOpen V ∧ x ∈ U ∧ y ∈ V ∧ closure U ∩ closure V = ∅ :=
sorry

theorem exercise_31_2 {X : Type*}
  [TopologicalSpace X] [NormalSpace X] {A B : Set X}
  (hA : IsClosed A) (hB : IsClosed B) (hAB : Disjoint A B) :
  ∃ (U V : Set X), IsOpen U ∧ IsOpen V ∧ A ⊆ U ∧ B ⊆ V ∧ closure U ∩ closure V = ∅ :=
sorry

theorem exercise_31_3 {α : Type*} [PartialOrder α]
  [TopologicalSpace α] (h : OrderTopology α) : RegularSpace α :=
sorry

theorem exercise_32_1 {X : Type*} [TopologicalSpace X]
  (hX : NormalSpace X) (A : Set X) (hA : IsClosed A) :
  NormalSpace {x // x ∈ A} :=
sorry

theorem exercise_32_2a
  {ι : Type*} {X : ι → Type*} [∀ i, TopologicalSpace (X i)]
  (h : ∀ i, Nonempty (X i)) (h2 : T2Space (Π i, X i)) :
  ∀ i, T2Space (X i) :=
sorry

theorem exercise_32_2b
  {ι : Type*} {X : ι → Type*} [∀ i, TopologicalSpace (X i)]
  (h : ∀ i, Nonempty (X i)) (h2 : RegularSpace (Π i, X i)) :
  ∀ i, RegularSpace (X i) :=
sorry

theorem exercise_32_2c
  {ι : Type*} {X : ι → Type*} [∀ i, TopologicalSpace (X i)]
  (h : ∀ i, Nonempty (X i)) (h2 : NormalSpace (Π i, X i)) :
  ∀ i, NormalSpace (X i) :=
sorry

theorem exercise_32_3 {X : Type*} [TopologicalSpace X]
  (hX : LocallyCompactSpace X) (hX' : T2Space X) :
  RegularSpace X :=
sorry

theorem exercise_33_7 {X : Type*} [TopologicalSpace X]
  (hX : LocallyCompactSpace X) (hX' : T2Space X) :
  ∀ x A, IsClosed A ∧ ¬ x ∈ A →
  ∃ (f : X → I), Continuous f ∧ f x = 1 ∧ f '' A = {0}
  :=
sorry

theorem exercise_33_8
  (X : Type*) [TopologicalSpace X] [RegularSpace X]
  (h : ∀ x A, IsClosed A ∧ ¬ x ∈ A →
  ∃ (f : X → I), Continuous f ∧ f x = (1 : I) ∧ f '' A = {0})
  (A B : Set X) (hA : IsClosed A) (hB : IsClosed B)
  (hAB : Disjoint A B)
  (hAc : IsCompact A) :
  ∃ (f : X → I), Continuous f ∧ f '' A = {0} ∧ f '' B = {1} :=
sorry

theorem exercise_34_9
  (X : Type*) [TopologicalSpace X] [CompactSpace X]
  (X1 X2 : Set X) (hX1 : IsClosed X1) (hX2 : IsClosed X2)
  (hX : X1 ∪ X2 = univ) (hX1m : MetrizableSpace X1)
  (hX2m : MetrizableSpace X2) : MetrizableSpace X :=
sorry

theorem exercise_38_6 {X : Type*}
  (X : Type*) [TopologicalSpace X] [RegularSpace X]
  (h : ∀ x A, IsClosed A ∧ ¬ x ∈ A →
  ∃ (f : X → I), Continuous f ∧ f x = (1 : I) ∧ f '' A = {0}) :
  IsConnected (univ : Set X) ↔ IsConnected (univ : Set (StoneCech X)) :=
sorry

theorem exercise_43_2 {X : Type*} [MetricSpace X]
  {Y : Type*} [MetricSpace Y] [CompleteSpace Y] (A : Set X)
  (f : X → Y) (hf : UniformContinuousOn f A) :
  ∃! (g : X → Y), ContinuousOn g (closure A) ∧
  UniformContinuousOn g (closure A) ∧ ∀ (x : A), g x = f x :=
sorry
