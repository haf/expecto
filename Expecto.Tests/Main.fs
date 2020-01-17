module Main

open Expecto
open Expecto.Logging

let logger = Log.create "Expecto.Tests"

[<EntryPoint>]
let main args =
  let writeResults = TestResults.writeNUnitSummary ("bin/Expecto.Tests.TestResults.xml", "Expecto.Tests")
  let test =
    Impl.testFromThisAssembly()
    |> Option.orDefault (TestList ([], Normal))
    |> Test.shuffle "."
  runTestsWithCLIArgs [Append_Summary_Handler(SummaryHandler writeResults)] args test