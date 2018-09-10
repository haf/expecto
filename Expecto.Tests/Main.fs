module Main

open Expecto

[<EntryPoint>]
let main args =
  let writeResults =
    TestResults.writeNUnitSummary
      ("bin/Expecto.Tests.TestResults.xml", "Expecto.Tests")
  let config = defaultConfig.appendSummaryHandler writeResults
  runTestsInAssembly config args