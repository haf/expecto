module Main

open Expecto
open Expecto.Logging
open OpenTelemetry.Resources
open OpenTelemetry
open OpenTelemetry.Trace
open System.Threading
open System.Diagnostics
open System

let serviceName = "Expecto.Tests"

let logger = Log.create serviceName


[<EntryPoint>]
let main args =

  let test =
    Impl.testFromThisAssembly()
    |> Option.orDefault (TestList ([], Normal))
    |> Test.shuffle "."
  runTestsWithCLIArgs [NUnit_Summary "bin/Expecto.Tests.TestResults.xml";] args test



