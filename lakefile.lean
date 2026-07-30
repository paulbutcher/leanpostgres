import Lake
open Lake DSL System

package leanpostgres where
  version := v!"0.1.0"
  license := "Apache-2.0"
  leanOptions := #[⟨`experimental.module, true⟩]

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

unsafe def getEnvImpl (name : String) : Option String :=
  unsafeBaseIO (IO.getEnv name)

@[implemented_by getEnvImpl]
opaque getEnv' (name : String) : Option String := none

/--
`-I` flags for the libpq headers: `LEANPOSTGRES_PQ_INCLUDE` override, else
`pkg-config`'s `includedir` (queried directly rather than via `--cflags`,
since `pkg-config` omits `-I` entirely for dirs it considers "standard" —
which the Lean toolchain's bundled clang/lld don't necessarily search).
-/
def libpqCflags : Array String :=
  match getEnv' "LEANPOSTGRES_PQ_INCLUDE" |>.orElse (fun _ => pkgConfigVar "includedir") with
  | some dir => #["-I", dir]
  | none => #[]

/--
Full path to the system `libpq` shared library, linked in by path rather
than as a bare `-lpq`/`-L<dir>` pair. `pkg-config`'s resolved `libdir` here
is frequently *also* a standard system library directory (e.g.
`/usr/lib/aarch64-linux-gnu`); adding it as an early `-L` search path would
take priority over — and can shadow — the Lean toolchain's own bundled
glibc directories later in the link line, breaking the final executable
link in a way that's silent and highly toolchain/platform-specific to
debug (it manifests as missing glibc-internal symbols, not anything
libpq-related). Linking by exact path sidesteps that entirely.
-/
target libpq.so _pkg : FilePath := do
  let path := match getEnv' "LEANPOSTGRES_PQ_LIB" |>.orElse (fun _ => pkgConfigVar "libdir") with
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

/-!
Also built as a shared library, loaded via `--load-dynlib`.
`Postgres.FFI`'s `initialize` block runs `leanpostgres_initialize` as
soon as any module imports it — including `lean`'s own compilation of
downstream modules in this library — so the native symbol must be
dlopen-able at that point, not just linked into a final executable.
-/
target leanpostgres.dynlib pkg : Dynlib := do
  let libFile := pkg.buildDir / "bindings" / nameToSharedLib "leanpostgres"
  let oJob ← leanpostgres.o.fetch
  let pqJob ← libpq.so.fetch
  buildLeanSharedLib "leanpostgres" libFile #[oJob, pqJob] #[]

@[default_target]
lean_lib Postgres where
  -- Needed so importers of `Postgres.FFI` can run its `initialize` block
  -- via a dynlib of the module's own compiled code, not just at final link time.
  precompileModules := true
  moreLinkObjs := #[leanpostgres.o, libpq.so]
  dynlibs := #[leanpostgres.dynlib]

-- Test-support code (the `TestM` success/failure-recording framework), kept as its own library
-- target — under the package root like `Postgres` above, not under `tests/` — rather than folded
-- into `testMain`'s exe root, so `TestMain.lean` can `import` it like any other module. Sharing
-- `tests/` as `srcDir` with the `testMain` exe below confuses Lake's target/module ownership
-- (it reports a generic "bad imports" error with no further detail), hence the separate root.
lean_lib PostgresTest

@[test_driver]
lean_exe testMain where
  root := `TestMain
  srcDir := "tests"
  needs := #[PostgresTest]
