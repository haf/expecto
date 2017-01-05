module Expecto.SummaryTests
#nowarn "46"

open Expecto
open System

[<Tests>]
let tests =
  testList "summary tests" [
    testCase "can log empty summary" <| fun _ ->
      let text = 
        Expecto.Impl.createSummaryText
          { passed = []
            ignored = []
            failed = []
            errored = []
            duration = TimeSpan.MinValue }
          Logging.LogLevel.Info

      Expect.equal (text.ToString()) "Passed:  0\nIgnored: 0\nFailed:  0\nErrored: 0" "empty"

    testCase "can log one passed test" <| fun _ ->
      let text = 
        Expecto.Impl.createSummaryText
          { passed = ["test 1"]
            ignored = []
            failed = []
            errored = []
            duration = TimeSpan.MinValue }
          Logging.LogLevel.Info

      Expect.equal (text.ToString()) "Passed:  1\n\ttest 1\nIgnored: 0\nFailed:  0\nErrored: 0" "one passed test"

    testCase "can log 10 passed test" <| fun _ ->
      let text = 
        Expecto.Impl.createSummaryText
          { passed = [1..10] |> List.map (fun x -> "test " + x.ToString())
            ignored = []
            failed = []
            errored = []
            duration = TimeSpan.MinValue }
          Logging.LogLevel.Info

      Expect.equal (text.ToString()) "Passed:  10\n\ttest 1\n\ttest 2\n\ttest 3\n\ttest 4\n\ttest 5\n\ttest 6\n\ttest 7\n\ttest 8\n\ttest 9\n\ttest 10\nIgnored:  0\nFailed:   0\nErrored:  0" "one passed test"
  ]