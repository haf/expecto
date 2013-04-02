module Fuchu.MbUnit

open Fuchu

/// Converts Fuchu tests to MbUnit tests
let rec FuchuTestToMbUnit name =
    function
    | TestCase t -> MbUnit.Framework.TestCase(name, Gallio.Common.Action t) :> MbUnit.Framework.Test
    | TestLabel(n, t) -> FuchuTestToMbUnit n t
    | TestList tests -> 
        let mbunitTests = Seq.map (FuchuTestToMbUnit name) tests
        let suite = MbUnit.Framework.TestSuite name
        Seq.iter suite.Children.Add mbunitTests
        suite :> MbUnit.Framework.Test

/// Scans Fuchu tests from an assembly an converts them to MbUnit tests
[<CompiledName("FromAssembly")>]
let fromAssembly (testAssembly: System.Reflection.Assembly) =
    seq {
        let tests = Fuchu.Impl.testFromAssembly testAssembly |> Option.toList
        let t = TestList tests
        yield FuchuTestToMbUnit "" t
    }
