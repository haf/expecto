module Fuchu.MbUnit

open Fuchu

let rec FuchuTestToMbUnit name =
    function
    | TestCase t -> MbUnit.Framework.TestCase(name, Gallio.Common.Action t) :> MbUnit.Framework.Test
    | TestLabel(n, t) -> FuchuTestToMbUnit n t
    | TestList tests -> 
        let mbunitTests = Seq.map (FuchuTestToMbUnit name) tests
        let suite = MbUnit.Framework.TestSuite name
        Seq.iter suite.Children.Add mbunitTests
        suite :> MbUnit.Framework.Test