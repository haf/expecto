module Main

open Expecto
open Expecto.Logging
open OpenTelemetry.Resources
open OpenTelemetry
open OpenTelemetry.Trace
open System.Threading
open System.Diagnostics

let serviceName = "Expecto.Tests"

let logger = Log.create serviceName


let resourceBuilder =
    ResourceBuilder
        .CreateDefault()
        .AddService(serviceName = serviceName)



[<EntryPoint>]
let main args =
  let activitySource = new ActivitySource(serviceName)
  use traceProvider =
          Sdk
              .CreateTracerProviderBuilder()
              .AddSource(serviceName)
              .SetResourceBuilder(resourceBuilder )
              .AddOtlpExporter()
              .Build()
  let tracer = traceProvider.GetTracer(serviceName)
  // use span = tracer.StartActiveSpan("Expecto.main")
  use span = tracer.StartRootSpan("Expecto.main")
  let test =
    Impl.testFromThisAssembly()
    |> Option.orDefault (TestList ([], Normal))
    |> OpenTelemetry.addOpenTelemetry_SpanPerTest Impl.ExpectoConfig.defaultConfig activitySource
    |> Test.shuffle "."
  runTestsWithCLIArgs [NUnit_Summary "bin/Expecto.Tests.TestResults.xml"] args test



