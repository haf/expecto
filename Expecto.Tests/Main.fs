module Main

open Expecto
open Expecto.Logging
open Expecto.Logging.Message

let logger = Log.create "Expecto.Tests"

[<EntryPoint>]
let main args =
  let writeResults = TestResults.writeNUnitSummary ("bin/Expecto.Tests.TestResults.xml", "Expecto.Tests")
  let config = defaultConfig.appendSummaryHandler writeResults
  runTestsInAssembly config args

  // logger.info (eventX "Starting Expecto tests {c}, console={con}" >> setField "c" ANSIOutputWriter.colour256 >> setField "con" System.Console.BackgroundColor)
