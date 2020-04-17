namespace Expecto

open System.Text
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
open BenchmarkDotNet.Reports
open BenchmarkDotNet.Validators
open System.Globalization

[<AutoOpen>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module BenchmarkDotNet =

  type BenchmarkAttribute = Attributes.BenchmarkAttribute
  type CleanupAttribute = Attributes.GlobalCleanupAttribute
  type SetupAttribute = Attributes.GlobalSetupAttribute

  type BenchmarkConfig =
    { columnProviders: IColumnProvider list
      hardwareCounters: HardwareCounter list
      summaryStyle: SummaryStyle
      exporters : IExporter list
      loggers : ILogger list
      diagnosers : IDiagnoser list
      analysers : IAnalyser list
      jobs : Job list
      validators : IValidator list
      orderer : IOrderer
      unionRule : ConfigUnionRule
      filters : IFilter list
      artifactsPath : string
      logicalGroupRules : BenchmarkLogicalGroupRule list
      cultureInfo: CultureInfo
      options: ConfigOptions
    }
    interface IConfig with
      member x.GetColumnProviders() = Seq.ofList x.columnProviders
      member x.GetHardwareCounters() = Seq.ofList x.hardwareCounters
      member x.SummaryStyle = x.summaryStyle
      member x.GetExporters() = Seq.ofList x.exporters
      member x.GetLoggers() = Seq.ofList x.loggers
      member x.GetDiagnosers() = Seq.ofList x.diagnosers
      member x.GetAnalysers() = Seq.ofList x.analysers
      member x.GetJobs() = Seq.ofList x.jobs
      member x.GetValidators() = Seq.ofList x.validators
      member x.Orderer = x.orderer
      member x.UnionRule = x.unionRule
      member x.GetFilters() = Seq.ofList x.filters
      member x.ArtifactsPath = x.artifactsPath
      member x.GetLogicalGroupRules() = Seq.ofList x.logicalGroupRules
      member x.CultureInfo = x.cultureInfo
      member x.Options = x.options

  let private synchronisedLogger =
    let cl = ConsoleLogger.Default
    { new ILogger with
        member __.Write(kind, text) =
          cl.Write(kind, text)
        member __.WriteLine(kind, text) =
          cl.WriteLine (kind, text)
        member __.WriteLine () =
          cl.WriteLine()
        member __.Flush () =
          cl.Flush()
        member __.Id =
          cl.Id
        member __.Priority =
          cl.Priority
    }

  let benchmarkConfig =
    let def = DefaultConfig.Instance
    { columnProviders = def.GetColumnProviders() |> List.ofSeq
      hardwareCounters = def.GetHardwareCounters() |> List.ofSeq
      summaryStyle = def.SummaryStyle
      exporters = def.GetExporters() |> List.ofSeq
      loggers = [ synchronisedLogger ]
      diagnosers = def.GetDiagnosers() |> List.ofSeq
      analysers = def.GetAnalysers() |> List.ofSeq
      jobs = def.GetJobs() |> List.ofSeq
      validators = def.GetValidators() |> List.ofSeq
      orderer = def.Orderer
      unionRule = def.UnionRule
      filters = def.GetFilters() |> List.ofSeq
      artifactsPath = def.ArtifactsPath
      logicalGroupRules = def.GetLogicalGroupRules() |> List.ofSeq
      cultureInfo = def.CultureInfo
      options = def.Options
    }

  /// Run a performance test: pass the annotated type as a type param
  /// to this function call. Pass 'benchmarkConfig' as the config parameter –
  /// because this is a record, you can change it to suit your liking.
  /// NOTE: Now needs to be manually put in a testCase.
  let benchmark<'typ> config onSummary =
    BenchmarkRunner.Run<'typ>(config) |> onSummary