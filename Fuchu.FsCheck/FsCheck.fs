namespace Fuchu

open System
open global.FsCheck

[<AutoOpen; CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module FuchuFsCheck =
    open Fuchu
    open Fuchu.Helpers
    open Fuchu.Impl
    open global.FsCheck
    open global.FsCheck.Runner

    let internal (|Ignored|_|) (e: exn) =
        match e with
        | :? IgnoreException as e -> Some e
        | _ -> None

    let internal wrapRunner (r : IRunner) =
        { new IRunner with
            member x.OnStartFixture t = r.OnStartFixture t
            member x.OnArguments (ntest, args, every) = r.OnArguments (ntest, args, every)
            member x.OnShrink(args, everyShrink) = r.OnShrink(args, everyShrink)
            member x.OnFinished(name,testResult) = 
                let msg = onFinishedToString name testResult
                match testResult with
                | FsCheck.TestResult.True _ -> ()
                | FsCheck.TestResult.False (_,_,_, Outcome.Exception (Ignored e),_) -> raise e
                | _ -> failtest msg
        }

    let internal config = 
        { Config.Default with
            Runner = wrapRunner Config.Default.Runner }

    let testPropertyWithConfig (config: Config) name property = 
        let config =
            { config with
                Runner = wrapRunner config.Runner }
        testCase name <|
            fun _ ->
                ignore Runner.init.Value
                FsCheck.Check.One(name, config, property)
    
    let testProperty name = testPropertyWithConfig config name

type FsCheck =
    static member Property(name, property: Func<_,bool>) =
        testProperty name property.Invoke

    static member Property(name, property: Func<_,_,bool>) =
        testProperty name (fun a b -> property.Invoke(a,b))

    static member Property(name, property: Func<_,_,_,bool>) =
        testProperty name (fun a b c -> property.Invoke(a,b,c))

    static member Property(name, property: Func<_,_,_,_,bool>) =
        testProperty name (fun a b c d -> property.Invoke(a,b,c,d))