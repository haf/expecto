namespace Expecto.CSharp

open System.Runtime.CompilerServices
open Expecto

// When exposing Extension Methods, you should declare an assembly-level attribute (in addition to class and method)
[<assembly:Extension>]
do
  ()

[<AutoOpen; Extension>]
module ConfigExt =

  type Expecto.Impl.ExpectoConfig with

    [<Extension; CompiledName("AddNUnitSummary")>]
    member x.AddNUnitSummary(file, assemblyName) =
      x.appendSummaryHandler(TestResults.writeNUnitSummary file)

    [<Extension; CompiledName("AddJUnitSummary")>]
    member x.AddJUnitSummary(file, assemblyName) =
      x.appendSummaryHandler(TestResults.writeJUnitSummary file)
