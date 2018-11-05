namespace Expecto

open BenchmarkDotNet
open BenchmarkDotNet.Running
open BenchmarkDotNet.Configs
open BenchmarkDotNet.Analysers
open BenchmarkDotNet.Columns
open BenchmarkDotNet.Diagnosers
open BenchmarkDotNet.Exporters
open BenchmarkDotNet.Filters
open BenchmarkDotNet.Jobs
open BenchmarkDotNet.Loggers
open BenchmarkDotNet.Order
open BenchmarkDotNet.Validators

[<AutoOpen>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module BenchmarkDotNet =

  type BenchmarkAttribute = Attributes.BenchmarkAttribute
  type CleanupAttribute = Attributes.GlobalCleanupAttribute
  type SetupAttribute = Attributes.GlobalSetupAttribute

  type BenchmarkConfig =
    { columnProviders: IColumnProvider list
      hardwareCounters: HardwareCounter list
      summaryStyle: Reports.ISummaryStyle
      exporters : IExporter list
      loggers : ILogger list
      diagnosers : IDiagnoser list
      analysers : IAnalyser list
      jobs : Job list
      validators : IValidator list
      orderProvider : IOrderProvider
      unionRule : ConfigUnionRule
      keepFiles : bool
      filters : IFilter list
      artifactsPath : string
      logicalGroupRules : BenchmarkLogicalGroupRule list
    }
    interface IConfig with
      member x.GetColumnProviders() = Seq.ofList x.columnProviders
      member x.GetHardwareCounters() = Seq.ofList x.hardwareCounters
      member x.GetSummaryStyle() = x.summaryStyle
      member x.GetExporters() = Seq.ofList x.exporters
      member x.GetLoggers() = Seq.ofList x.loggers
      member x.GetDiagnosers() = Seq.ofList x.diagnosers
      member x.GetAnalysers() = Seq.ofList x.analysers
      member x.GetJobs() = Seq.ofList x.jobs
      member x.GetValidators() = Seq.ofList x.validators
      member x.GetOrderProvider() = x.orderProvider
      member x.UnionRule = x.unionRule
      member x.KeepBenchmarkFiles = x.keepFiles
      member x.GetFilters() = Seq.ofList x.filters
      member x.ArtifactsPath = x.artifactsPath
      member x.GetLogicalGroupRules() = Seq.ofList x.logicalGroupRules

  let private synchronisedLogger =
    let cl = ConsoleLogger.Default
    { new ILogger with
        member __.Write(kind, text) =
          cl.Write(kind, text)
        member __.WriteLine(kind, text) =
          cl.WriteLine (kind, text)
        member __.WriteLine () =
          cl.WriteLine()
    }

  let benchmarkConfig =
    let def = DefaultConfig.Instance
    { columnProviders = def.GetColumnProviders() |> List.ofSeq
      hardwareCounters = def.GetHardwareCounters() |> List.ofSeq
      summaryStyle = def.GetSummaryStyle()
      exporters = def.GetExporters() |> List.ofSeq
      loggers = [ synchronisedLogger ]
      diagnosers = def.GetDiagnosers() |> List.ofSeq
      analysers = def.GetAnalysers() |> List.ofSeq
      jobs = def.GetJobs() |> List.ofSeq
      validators = def.GetValidators() |> List.ofSeq
      orderProvider = def.GetOrderProvider()
      unionRule = def.UnionRule
      keepFiles = true
      filters = def.GetFilters() |> List.ofSeq
      artifactsPath = def.ArtifactsPath
      logicalGroupRules = def.GetLogicalGroupRules() |> List.ofSeq
    }

  /// Run a performance test: pass the annotated type as a type param
  /// to this function call. Pass 'benchmarkConfig' as the config parameter –
  /// because this is a record, you can change it to suit your liking.
  /// NOTE: Now needs to be manually put in a testCase.
  let benchmark<'typ> config onSummary =
    BenchmarkRunner.Run<'typ>(config) |> onSummary