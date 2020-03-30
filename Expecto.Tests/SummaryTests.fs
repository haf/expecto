module Expecto.SummaryTests
#nowarn "46"

open Expecto
open Impl
open System

let getRelevantPart (text:string) =
    text.Split([|"EXPECTO?! Summary..."|],StringSplitOptions.None).[1]
    |> fun s -> s.TrimStart()
    |> fun s -> s.Replace(" []","")

let testResult name =
  let test =
    { name = [name]
      test = Sync ignore
      state = Normal
      focusOn = false
      sequenced = Synchronous }
  let result = TestSummary.single Passed 0.0
  test, result

[<Tests>]
let tests =
  testList "summary tests" [
    testCase "can log empty summary" <| fun _ ->
      let text =
        Expecto.Impl.createSummaryText (JoinBy.Dot.asString)
          { results = []
            duration = TimeSpan.MinValue
            maxMemory = 0L
            memoryLimit = 0L
            timedOut = [] }
        |> getRelevantPart

      Expect.equal text "Passed:  0\nIgnored: 0\nFailed:  0\nErrored: 0" "empty"

    testCase "can log one passed test" <| fun _ ->
      let text =
        Expecto.Impl.createSummaryText (JoinBy.Dot.asString)
          { results = [testResult "test 1"]
            duration = TimeSpan.MinValue
            maxMemory = 0L
            memoryLimit = 0L
            timedOut = [] }
          |> getRelevantPart

      Expect.equal text "Passed:  1\n\ttest 1\nIgnored: 0\nFailed:  0\nErrored: 0" "one passed test"

    testCase "can log 9 passed test" <| fun _ ->
      let text =
        Expecto.Impl.createSummaryText (JoinBy.Dot.asString)
          { results = [1..9] |> List.map (fun x -> "test " + x.ToString() |> testResult)
            duration = TimeSpan.MinValue
            maxMemory = 0L
            memoryLimit = 0L
            timedOut = [] }
         |> getRelevantPart

      Expect.equal text "Passed:  9\n\ttest 1\n\ttest 2\n\ttest 3\n\ttest 4\n\ttest 5\n\ttest 6\n\ttest 7\n\ttest 8\n\ttest 9\nIgnored: 0\nFailed:  0\nErrored: 0" "one passed test"
  ]