namespace Expecto

open BenchmarkDotNet
open BenchmarkDotNet.Running
open BenchmarkDotNet.Configs
open BenchmarkDotNet.Analysers
open BenchmarkDotNet.Columns
open BenchmarkDotNet.Diagnosers
open BenchmarkDotNet.Exporters
open BenchmarkDotNet.Jobs
open BenchmarkDotNet.Loggers
open BenchmarkDotNet.Order
open BenchmarkDotNet.Validators
open Expecto
open Expecto.Logging
open Expecto.Logging.Message

[<AutoOpen; CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module BenchmarkDotNet =
  let logger = Log.create "Expecto.BenchmarkDotNet"

  type BenchmarkConfig =
    { columns : IColumn list
      exporters : IExporter list
      loggers : ILogger list
      diagnosers : IDiagnoser list
      analysers : IAnalyser list
      jobs : IJob list
      validators : IValidator list
      orderProvider : IOrderProvider
      unionRule : ConfigUnionRule
      keepFiles : bool }

    interface IConfig with
      member x.GetColumns() : IColumn seq = upcast x.columns
      member x.GetExporters() : seq<IExporter> = upcast x.exporters
      member x.GetLoggers() : seq<ILogger> = upcast x.loggers
      member x.GetDiagnosers() : seq<IDiagnoser> = upcast x.diagnosers
      member x.GetAnalysers() : seq<IAnalyser> = upcast x.analysers
      member x.GetJobs() : seq<IJob> = upcast x.jobs
      member x.GetValidators() : seq<IValidator> = upcast x.validators
      member x.GetOrderProvider() : IOrderProvider  = x.orderProvider
      member x.UnionRule : ConfigUnionRule = x.unionRule
      /// Determines if all auto-generated files should be kept or removed after running benchmarks
      member x.KeepBenchmarkFiles = x.keepFiles

  let private synchronisedLogger =
    let cl = ConsoleLogger.Default
    { new ILogger with
        member x.Write(kind, text) =
          Global.lockSem <| fun _ -> cl.Write(kind, text)
        member x.WriteLine(kind, text) =
          Global.lockSem <| fun _ -> cl.WriteLine (kind, text)
        member x.WriteLine () =
          Global.lockSem <| fun _ -> cl.WriteLine()
    }

  let benchmarkConfig =
    let def = DefaultConfig.Instance
    { columns = def.GetColumns() |> List.ofSeq
      exporters = def.GetExporters() |> List.ofSeq
      loggers = [ synchronisedLogger ]
      diagnosers = def.GetDiagnosers() |> List.ofSeq
      analysers = def.GetAnalysers() |> List.ofSeq
      jobs = def.GetJobs() |> List.ofSeq
      validators = def.GetValidators() |> List.ofSeq
      orderProvider = def.GetOrderProvider()
      unionRule = def.UnionRule
      keepFiles = true }

  /// Create a new performance test: pass the annotated type as a type param
  /// to this function call. Pass 'benchmarkConfig' as the config parameter –
  /// because this is a record, you can change it to suit your liking.
  let benchmark<'typ> testName config onSummary =
    testCase testName <| fun _ ->
      BenchmarkRunner.Run<'typ>(config)
      |> onSummary