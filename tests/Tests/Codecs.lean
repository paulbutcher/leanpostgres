/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module
import all Postgres.TextCodec
import Postgres.Array

open Postgres

set_option maxRecDepth 10000

/--
Together with `hex_nibbles_recompose` this is the whole per-byte content of the `bytea` hex
codec: `byteArrayToHex` emits `hexDigit (b >>> 4)` then `hexDigit (b &&& 0xF)`, and
`hexToByteArray?` reads them back with `hexValue?` before recombining. What these two theorems
leave uncovered is only the string plumbing around them, which `hexRoundTripsProperty` in
`Tests.CodecProperties` covers.
-/
theorem hex_digit_round_trip :
    ∀ n < 16, TextCodec.hexValue? (TextCodec.hexDigit n.toUInt8) = some n.toUInt8 := by
  decide

/-- Splitting a byte into nibbles and recombining them is the identity, for every byte. -/
theorem hex_nibbles_recompose :
    ∀ n < 256, ((n.toUInt8 >>> (4 : UInt8)) <<< (4 : UInt8) ||| (n.toUInt8 &&& (0xF : UInt8)))
      = n.toUInt8 := by
  decide

/-- The `t`/`f` array-element encoding of `Bool` is invertible. -/
theorem arrayElem_bool_round_trip :
    ∀ b : Bool, ArrayElem.decode? (ArrayElem.encode b) = some b := by
  decide
