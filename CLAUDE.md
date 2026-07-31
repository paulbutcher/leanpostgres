# CLAUDE.md

Project-specific guidance for Claude Code when working in this repo.

## Verifying Lean changes

- After editing a `.lean` file, verify it with `mcp__lean-lsp__lean_diagnostic_messages`
  (or other lean-lsp-mcp tools, e.g. `lean_goal`/`lean_multi_attempt` for
  interactive proof/termination work).
- Ignore the editor's `<ide_diagnostics>` hook output.
- Before considering a task complete, run both `lake build` and `lake test` from the
  repo root as the final ground truth.
- If a change adds or removes an `import`, use `mcp__lean-lsp__lean_build`
  instead of (or in addition to) plain `lake build`.

## Commenting

- Only add comments which say something over and above what the source code already
  says. Avoid comments which restate what can be derived easily by reading the code.
- This includes header comments for both files and functions. Avoid them unless they
  add real value.
- Do add comments when it's not clear *why* the code is doing what it does just 
  from reading the code.
- Don't refer to previous implementations or rejected designs unless doing so is
  essential to understand the code.
- Don't mention project plans, milestones, ticket numbers, or anything similar in
  comments. Comments should remain valid years ahead, when people will not care 
  about the process that led to them.
- Don't add comments explaining Lean language features or quirks. Readers of this
  project understand Lean and don't need it explaining to them.
- All files should start with a copyright statement containing:
  Copyright (c) 2026 Paul Butcher. All rights reserved.
  Released under Apache 2.0 license as described in the file LICENSE.
- Never use an emdash (—). Wherever you might use one, use either a comma or a
  semicolon instead.
  
## Environment

- Installing OS packages should be done by infrastructure external to this project so
  it's controlled. Therefore, never install any OS packages. Always ask before doing
  so. This does not apply to Lean packages (i.e. defined by lake-manifest or lakefile).

## Process

- If the user asks a question, JUST answer it. Do not take a question as an
  instruction or recommendation; take it literally, answer it, and stop.
