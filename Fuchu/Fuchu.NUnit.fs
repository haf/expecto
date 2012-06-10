namespace Fuchu

module NUnit =
    open Fuchu
    open System
    open System.Reflection
    open Fuchu.Helpers
    open Fuchu.XunitHelpers
    
    let private NUnitAttr = sprintf "NUnit.Framework.%sAttribute"

    let NUnitTestToFuchu : Type -> Test = 
        TestToFuchu 
            (NUnitAttr "Ignore")
            (NUnitAttr "Test")
            (NUnitAttr "SetUp") 
            (NUnitAttr "TearDown")
            (NUnitAttr "TestFixtureSetUp")
            (fun _ -> "")
