module Expecto.SummaryTests
#nowarn "46"

open Expecto
open Impl
open System

let getRelevantPart (text:string) =
    text.Split([|"EXPECTO?! Summary..."|],StringSplitOptions.None).[1]
    |> fun s -> s.TrimStart()
    |> fun s -> s.Replace(" []","")

let testResult name = {
  name = name
  location = SourceLocation.empty
  result = Passed
  duration = TimeSpan.MinValue
}

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
        |> getRelevantPart

      Expect.equal text "Passed:  0\nIgnored: 0\nFailed:  0\nErrored: 0" "empty"

    testCase "can log one passed test" <| fun _ ->
      let text =
        Expecto.Impl.createSummaryText
          { passed = [testResult "test 1"]
            ignored = []
            failed = []
            errored = []
            duration = TimeSpan.MinValue }
          |> getRelevantPart

      Expect.equal text "Passed:  1\n\ttest 1\nIgnored: 0\nFailed:  0\nErrored: 0" "one passed test"

    testCase "can log 10 passed test" <| fun _ ->
      let text =
        Expecto.Impl.createSummaryText
          { passed = [1..10] |> List.map (fun x -> "test " + x.ToString() |> testResult)
            ignored = []
            failed = []
            errored = []
            duration = TimeSpan.MinValue }
         |> getRelevantPart

      Expect.equal text "Passed:  10\n\ttest 1\n\ttest 2\n\ttest 3\n\ttest 4\n\ttest 5\n\ttest 6\n\ttest 7\n\ttest 8\n\ttest 9\n\ttest 10\nIgnored:  0\nFailed:   0\nErrored:  0" "one passed test"
  ]