#!/usr/bin/env bash
#
# Copyright (c) 2026 Paul Butcher. All rights reserved.
# Released under Apache 2.0 license as described in the file LICENSE.
#
# Builds the `linkage/` package, a consumer that derives `Row`, `ResultColumn`, `QueryParam`,
# `ToBinary` and `FromBinary`, and uses the JSON binary codecs and `viaJson`, and checks that the
# executable it produces carries none of the Lean package.
#
# Every module of the Lean package defines an `initialize_Lean_<Module>` symbol, and neither
# `libInit.a` nor `libStd.a` defines one, so counting them says exactly whether any of `libLean.a`
# was pulled in. `-lLean` itself is on the link line of every executable Lake builds, hello world
# included, so its presence there says nothing; what it resolves to does.

set -euo pipefail

repo="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
exe="$repo/linkage/.lake/build/bin/linkage"

(cd "$repo/linkage" && lake build)

leanSymbols=$(nm "$exe" | grep -c "initialize_Lean_" || true)
if [ "$leanSymbols" -ne 0 ]; then
  echo "FAIL: the Lean package is linked in ($leanSymbols module initializers)" >&2
  nm "$exe" | grep "initialize_Lean_" | head -20 >&2
  exit 1
fi

"$exe"

echo "linkage: no Lean package symbols, $(du -h "$exe" | cut -f1) executable"
