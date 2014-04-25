namespace Fuchu

open System
open global.FsCheck.Fluent

[<AutoOpen; CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module FuchuFsCheck =
    open Fuchu
    open Fuchu.Helpers
    open Fuchu.Impl
    open global.FsCheck
    open global.FsCheck.Runner

    let internal runner = 
        { new IRunner with
            member x.OnStartFixture t = ()
            member x.OnArguments (ntest,args, every) = ()
            member x.OnShrink(args, everyShrink) = ()
            member x.OnFinished(name,testResult) = 
                let msg = onFinishedToString name testResult
                match testResult with
                | TestResult.True _ -> ()
                | _ -> failtest msg
        }

    let internal config = 
        { Config.Default with
            Runner = runner }

    let testPropertyWithConfig (config: Config) name property = 
        let config =
            { config with
                Runner = runner }
        testCase name <|
            fun _ ->
                ignore Runner.init.Value
                FsCheck.Check.One(name, config, property)
    
    let testProperty name = testPropertyWithConfig config name

type FsCheck =
    static member Property(name, property: SpecBuilder<_>) =
        testProperty name (property.Build())

    static member Property(name, property: SpecBuilder<_,_>) =
        testProperty name (property.Build())

    static member Property(name, property: SpecBuilder<_,_,_>) =
        testProperty name (property.Build())

    static member Property(name, property: Func<_,bool>) =
        testProperty name property.Invoke

    static member Property(name, property: Func<_,_,bool>) =
        testProperty name (fun a b -> property.Invoke(a,b))

    static member Property(name, property: Func<_,_,_,bool>) =
        testProperty name (fun a b c -> property.Invoke(a,b,c))

    static member Property(name, property: Func<_,_,_,_,bool>) =
        testProperty name (fun a b c d -> property.Invoke(a,b,c,d))