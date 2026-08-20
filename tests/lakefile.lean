/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Lake
open Lake DSL

require leanpostgres from ".."
require plausible from git
  "https://github.com/leanprover-community/plausible" @ "v4.33.0"

package «leanpostgres-tests» where
  leanOptions := #[⟨`experimental.module, true⟩]

-- Test-support code (the `TestM` success/failure-recording framework), kept as its own library
-- target rather than folded into `testMain`'s exe root, so `TestMain.lean` can `import` it like
-- any other module.
@[default_target]
lean_lib PostgresTest where
  precompileModules := true

-- The test bodies themselves, split into one file per feature area under `Tests/`. A `lean_exe`'s
-- root module isn't a general import search path, so `TestMain.lean` can't just `import` sibling
-- files directly; they need to be a proper library target like this one for their `.olean`s to end
-- up on the search path. Unlike `PostgresTest` above, there's no single root file importing every
-- submodule (nothing needs to import the whole group at once), so `globs` selects every file under
-- the directory directly rather than following imports from a root.
lean_lib Tests where
  globs := #[`Tests.+]

@[default_target, test_driver]
lean_exe testMain where
  root := `TestMain
  needs := #[PostgresTest, Tests]
