namespace Expecto.CSharp

open System.Runtime.InteropServices
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
      x.appendSummaryHandler(TestResults.writeNUnitSummary(file, assemblyName))

    /// If using this with gitlab, set the third parameter 'handleErrorsLikeFailures' to true.
    [<Extension; CompiledName("AddJUnitSummary")>]
    member x.AddJUnitSummary(file, assemblyName, [<Optional;DefaultParameterValue(false)>] handleErrorsLikeFailures) =
      x.appendSummaryHandler(TestResults.writeJUnitSummary(file, assemblyName, handleErrorsLikeFailures))
