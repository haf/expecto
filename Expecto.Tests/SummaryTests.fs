module Expecto.SummaryTests
#nowarn "46"

open Expecto
open System

[<Tests>]
let tests =
  testList "all" [
    testCase "can log empty summary" <| fun _ ->
      let text = 
        Expecto.Impl.createSummaryText
          { passed = []
            ignored = []
            failed = []
            errored = []
            duration = TimeSpan.MinValue }
          Logging.LogLevel.Info

      Expect.equal (text.ToString()) "Passed: 0\nIgnored: 0\nFailed: 0\nErrored: 0" "empty"
  ]