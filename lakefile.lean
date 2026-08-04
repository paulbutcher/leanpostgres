/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Lake
open Lake DSL System

package leanpostgres where
  version := v!"0.2.0"
  license := "Apache-2.0"
  leanOptions := #[⟨`experimental.module, true⟩]
  builtinLint := true

require plausible from git
  "https://github.com/leanprover-community/plausible" @ "v4.32.0"

/-- Runs `pkg-config --variable=<name> libpq`. `none` if pkg-config doesn't know libpq. -/
unsafe def pkgConfigVarImpl (name : String) : Option String :=
  match unsafeBaseIO (EIO.toBaseIO (IO.Process.output
      { cmd := "pkg-config", args := #[s!"--variable={name}", "libpq"] })) with
  | .ok out =>
    if out.exitCode == 0 then
      let line := (out.stdout.splitOn "\n").headD ""
      if line.isEmpty then none else some line
    else none
  | .error _ => none

@[implemented_by pkgConfigVarImpl]
opaque pkgConfigVar (name : String) : Option String := none

/--
Runs `brew --prefix libpq`, for platforms where Homebrew's `libpq` is
keg-only and doesn't register with `pkg-config` unless the user manually
links it (the default install). `none` if `brew` isn't on `PATH` or doesn't
know about `libpq`.
-/
unsafe def brewPrefixImpl (_ : Unit) : Option String :=
  match unsafeBaseIO (EIO.toBaseIO (IO.Process.output
      { cmd := "brew", args := #["--prefix", "libpq"] })) with
  | .ok out =>
    if out.exitCode == 0 then
      let line := (out.stdout.splitOn "\n").headD ""
      if line.isEmpty then none else some line
    else none
  | .error _ => none

@[implemented_by brewPrefixImpl]
opaque brewPrefix (_ : Unit) : Option String := none

unsafe def getEnvImpl (name : String) : Option String :=
  unsafeBaseIO (IO.getEnv name)

@[implemented_by getEnvImpl]
opaque getEnv' (name : String) : Option String := none

/--
`-I` flags for the libpq headers: `LEANPOSTGRES_PQ_INCLUDE` override, else
`pkg-config`'s `includedir` (queried directly rather than via `--cflags`,
since `pkg-config` omits `-I` entirely for dirs it considers "standard",
which the Lean toolchain's bundled clang/lld don't necessarily search), else
(macOS) `brew --prefix libpq`'s `include` directory.
-/
def libpqCflags : Array String :=
  let dir := getEnv' "LEANPOSTGRES_PQ_INCLUDE"
    |>.orElse (fun _ => pkgConfigVar "includedir")
    |>.orElse (fun _ => brewPrefix () |>.map (· ++ "/include"))
  match dir with
  | some dir => #["-I", dir]
  | none => #[]

/--
Full path to the system `libpq` shared library, linked in by path rather
than as a bare `-lpq`/`-L<dir>` pair. `pkg-config`'s resolved `libdir` here
is frequently *also* a standard system library directory (e.g.
`/usr/lib/aarch64-linux-gnu`); adding it as an early `-L` search path would
take priority over, and can shadow, the Lean toolchain's own bundled
glibc directories later in the link line, breaking the final executable
link in a way that's silent and highly toolchain/platform-specific to
debug (it manifests as missing glibc-internal symbols, not anything
libpq-related). Linking by exact path sidesteps that entirely.

Resolution order matches `libpqCflags`: `LEANPOSTGRES_PQ_LIB` override, else
`pkg-config`'s `libdir`, else (macOS) `brew --prefix libpq`'s `lib` directory.
-/
target libpq.so _pkg : FilePath := do
  let dir := getEnv' "LEANPOSTGRES_PQ_LIB"
    |>.orElse (fun _ => pkgConfigVar "libdir")
    |>.orElse (fun _ => brewPrefix () |>.map (· ++ "/lib"))
  let path := match dir with
    | some dir => (dir : FilePath) / nameToSharedLib "pq"
    | none => nameToSharedLib "pq"
  return Job.pure path

target leanpostgres.o pkg : FilePath := do
  let oFile := pkg.buildDir / "bindings" / "leanpostgres.o"
  let srcFile := pkg.dir / "bindings" / "leanpostgres.c"
  let srcJob ← inputTextFile srcFile
  let leanIncludeDir ← getLeanIncludeDir
  let flags := #["-I", leanIncludeDir.toString] ++ libpqCflags
  buildO oFile srcJob flags #[]

/--
Also built as a shared library, loaded via `--load-dynlib`, needed alongside `leanpostgres.o`
above, which alone only covers the final executable link.
-/
target leanpostgres.dynlib pkg : Dynlib := do
  let libFile := pkg.buildDir / "bindings" / nameToSharedLib "leanpostgres"
  let oJob ← leanpostgres.o.fetch
  let pqJob ← libpq.so.fetch
  buildLeanSharedLib "leanpostgres" libFile #[oJob, pqJob] #[]

@[default_target]
lean_lib Postgres where
  -- Needed so importers of `Postgres.FFI` get its native code before the final executable link.
  precompileModules := true
  moreLinkObjs := #[leanpostgres.o, libpq.so]
  dynlibs := #[leanpostgres.dynlib]

-- Test-support code (the `TestM` success/failure-recording framework), kept as its own library
-- target, under the package root like `Postgres` above, not under `tests/`, rather than folded
-- into `testMain`'s exe root, so `TestMain.lean` can `import` it like any other module.
lean_lib PostgresTest

@[test_driver]
lean_exe testMain where
  root := `TestMain
  srcDir := "tests"
  needs := #[PostgresTest]
