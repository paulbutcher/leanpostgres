/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Postgres
import PostgresTest.Framework
import Tests.Connection
import Tests.Statements
import Tests.CoreTypes
import Tests.Transactions
import Tests.RowDeriving
import Tests.BlobDeriving
import Tests.DomainTypes
import Tests.Metadata
import Tests.Pool
import Tests.CodecProperties

open Postgres
open Postgres.Test

/-- Runs the whole suite, returning the process exit code (`0` on success, `1` if anything failed). -/
def runTests (report : String → IO Unit := IO.println) (verbose : Bool := false) : IO UInt32 := do
  let config : Config := { verbose, report }
  let headerRef ← IO.mkRef (none : Option String)
  let conn ← «open» ""

  let (_, finalStats) ← (((do
    testFFIInitialized
    testConnectSuccess
    testConnectFailure
    testStatementLifecycle conn
    testExecScriptMultiStatement conn
    testExecScriptImplicitTransaction conn
    testMalformedStatementError conn
    testCoreTypeRoundTrip conn
    testTupleRowIteration conn
    testInterpolationMacros conn
    testTransactionOptionsCombinations conn
    testTransactionCommitAndRollback conn
    testUniqueViolationSqlstate conn
    testPersonRowDeriving conn
    testNullablePersonRowDeriving conn
    testProductRowDeriving conn
    testAllOptionalRowDeriving conn
    testEmptyRowDeriving conn
    testWrapperTypeDeriving conn
    testCoordinateRowDeriving conn
    testEmailDeriving conn
    testNonEmptyStringQueryParamDeriving conn
    testBlobDeriving
    testBlobDerivingProperties
    testNumericRoundTrip conn
    testUuidRoundTrip conn
    testDateRoundTrip conn
    testTimeRoundTrip conn
    testTimestampRoundTrip conn
    testTimestamptzRoundTrip conn
    testArrayRoundTrip conn
    testCodecProperties
    testColumnMetadata conn
    testColumnCount conn
    testCommandMetadata conn
    testPoolConcurrencyBound
    testPoolReleaseOnThrow
    testPoolSizeInvariantProperty
  ).run config).run headerRef).run { successes := 0, failures := 0 }

  report ""
  report "=== Test Summary ==="
  report s!"Successes: {finalStats.successes}"
  report s!"Failures:  {finalStats.failures}"
  report s!"Total:     {finalStats.total}"

  if finalStats.failures == 0 then
    report "\nAll tests passed!"
    return 0
  else
    report s!"\n{finalStats.failures} test(s) failed!"
    return 1

def main (args : List String) : IO UInt32 :=
  runTests (verbose := args.contains "--verbose")
