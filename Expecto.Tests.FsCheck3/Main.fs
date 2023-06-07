module Main

open Expecto
open Expecto.Logging

let logger = Log.create "Expecto.Tests"

[<EntryPoint>]
let main args =
  let test =
    Impl.testFromThisAssembly()
    |> Option.orDefault (TestList ([], Normal))
    |> Test.shuffle "."
  runTestsWithCLIArgs [NUnit_Summary "bin/Expecto.Tests.TestResults.xml"] args test
