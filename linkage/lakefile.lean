/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Lake
open Lake DSL

require leanpostgres from ".."

package «leanpostgres-linkage» where
  leanOptions := #[⟨`experimental.module, true⟩]

-- A consumer of the library and nothing else, so that what it links is what a downstream program
-- links. `scripts/check-linkage.sh` builds it and inspects the result.
@[default_target]
lean_exe linkage where
  root := `Linkage
