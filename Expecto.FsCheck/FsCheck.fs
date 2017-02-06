namespace Expecto

open System
open FsCheck
open Expecto

[<AutoOpen; CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module ExpectoFsCheck =

  let private propertyTest methodName focusState configs name property =

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

    let test config =
      let config = {
        MaxTest = config.maxTest
        MaxFail = 1000
        Replay = Option.map Random.StdGen config.replay
        Name = String.Empty
        StartSize = config.startSize
        EndSize = config.endSize
        QuietOnSuccess = true
        Every = fun _ _ -> String.Empty
        EveryShrink = fun _ -> String.Empty
        Arbitrary = config.arbitrary
        Runner = runner }
      Check.One(String.Empty, config, property)
      |> async.Return

    let testCode =
      match configs with
      | None ->
        AsyncFsCheck (None, None, test)
      | Some (testConfig, stressConfig) ->
        AsyncFsCheck (Some testConfig, Some stressConfig, test)
    TestLabel(name, TestCase (testCode, focusState), focusState)

  /// Builds a test property with config
  let testPropertyWithConfigs testConfig stressConfig name =
    propertyTest "ftestPropertyWithConfig" Normal
                 (Some(testConfig,stressConfig)) name

  /// Builds a test property with config that will be ignored by Expecto.
  let ptestPropertyWithConfigs testConfig stressConfig name =
    propertyTest "ftestPropertyWithConfig" Pending
                 (Some(testConfig,stressConfig)) name

  /// Builds a test property with config that will make Expecto to
  /// ignore other unfocused tests.
  let ftestPropertyWithConfigs stdGen testConfig stressConfig name =
    let testConfig = { testConfig with replay = Some stdGen }
    let stressConfig = { stressConfig with replay = Some stdGen }
    propertyTest "ftestPropertyWithConfig" Focused
                 (Some(testConfig,stressConfig)) name

  /// Builds a test property with config
  let testPropertyWithConfig config name =
    propertyTest "ftestPropertyWithConfig" Normal (Some(config,config)) name

  /// Builds a test property with config that will be ignored by Expecto.
  let ptestPropertyWithConfig config name =
    propertyTest "ftestPropertyWithConfig" Pending (Some(config,config)) name

  /// Builds a test property with config that will make Expecto to
  /// ignore other unfocused tests.
  let ftestPropertyWithConfig stdGen config name =
    let config = { config with replay = Some stdGen }
    propertyTest "ftestPropertyWithConfig" Focused (Some(config,config)) name

  /// Builds a test property.
  let testProperty name =
    propertyTest "ftestProperty" Normal None name

  /// Builds a test property that will be ignored by Expecto.
  let ptestProperty name =
    propertyTest "ftestProperty" Pending None name

  /// Builds a test property that will make Expecto to ignore other unfocused tests.
  let ftestProperty stdGen name =
    let config = { FsCheckConfig.defaultConfig with replay = Some stdGen }
    propertyTest "ftestProperty" Focused (Some(config,config)) name


type FsCheck =
  static member Property(name, property: Func<_,bool>) =
      testProperty name property.Invoke

  static member Property(name, property: Func<_,_,bool>) =
      testProperty name (fun a b -> property.Invoke(a,b))

  static member Property(name, property: Func<_,_,_,bool>) =
      testProperty name (fun a b c -> property.Invoke(a,b,c))

  static member Property(name, property: Func<_,_,_,_,bool>) =
      testProperty name (fun a b c d -> property.Invoke(a,b,c,d))
