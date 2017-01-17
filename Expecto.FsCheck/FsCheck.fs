namespace Expecto

open System
open FsCheck
open Expecto

[<AutoOpen; CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module ExpectoFsCheck =

  let private propertyTest methodName focusState config name property =

    /// This running plugs into FsCheck and gives us the ability to get called
    /// by the framework on start/finish of fixtures.
    let runner =
      { new IRunner with
          member __.OnStartFixture _ = ()
          member __.OnArguments(_,_,_) = ()
          member __.OnShrink(_,_) = ()
          member __.OnFinished(_, testResult) =
            let numTests i = if i = 1 then "1 test" else sprintf "%i tests" i
            match testResult with
            | TestResult.True (_testData,_b) ->
              ()
            | TestResult.False
                (_,_,_,Outcome.Exception (:? IgnoreException as e),_) ->
              raise e
            | TestResult.False
              (data, original, shrunk, outcome, Random.StdGen (std,gen)) ->

              sprintf
                "Failed after %s. Parameters: %s Result: %A\n%s (%i,%i)"
                (numTests data.NumberOfTests)
                (String.Join(" ", List.map (sprintf "%A") original) +
                  if data.NumberOfShrinks>0 then
                    " (Shrunk: " +
                        String.Join(" ",List.map (sprintf "%A") shrunk) + ")"
                  else "")
                outcome
                ("Focus on failure: "+methodName)
                std gen
              |> FailedException |> raise
            | TestResult.Exhausted data ->
              "Exhausted after " + (numTests data.NumberOfTests) + "."
              |> FailedException |> raise
      }

    let config =
      { config with
          Runner = runner }

    let test = async {
      Check.One(String.Empty, config, property)
    }

    TestLabel(name, TestCase (Async test, focusState), focusState)

  /// Builds a test property with config
  let testPropertyWithConfig config name =
    propertyTest "ftestPropertyWithConfig" Normal config name

  /// Builds a test property with config that will be ignored by Expecto.
  let ptestPropertyWithConfig config name =
    propertyTest "ftestPropertyWithConfig" Pending config name

  /// Builds a test property with config that will make Expecto to
  /// ignore other unfocused tests.
  let ftestPropertyWithConfig stdGen config name =
    let config =
      { config with
          Replay = Random.StdGen stdGen |> Some }
    propertyTest "ftestPropertyWithConfig" Focused config name

  /// Builds a test property.
  let testProperty name =
    propertyTest "ftestProperty" Normal Config.Default name

  /// Builds a test property that will be ignored by Expecto.
  let ptestProperty name =
    propertyTest "ftestProperty" Pending Config.Default name

  /// Builds a test property that will make Expecto to ignore other unfocused tests.
  let ftestProperty stdGen name =
    let config =
      { Config.Default with
          Replay = Random.StdGen stdGen |> Some }
    propertyTest "ftestProperty" Focused config name


type FsCheck =
  static member Property(name, property: Func<_,bool>) =
      testProperty name property.Invoke

  static member Property(name, property: Func<_,_,bool>) =
      testProperty name (fun a b -> property.Invoke(a,b))

  static member Property(name, property: Func<_,_,_,bool>) =
      testProperty name (fun a b c -> property.Invoke(a,b,c))

  static member Property(name, property: Func<_,_,_,_,bool>) =
      testProperty name (fun a b c d -> property.Invoke(a,b,c,d))
