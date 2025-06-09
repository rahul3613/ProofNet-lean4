import Lake
open Lake DSL

package «proofNet-lean4» {
  -- add any package configuration options here
}

require mathlib from git
  "https://github.com/leanprover-community/mathlib4.git" @ "v4.20.0"

@[default_target]
lean_lib «ProofNetLean4» {
  -- add any library configuration options here
}
