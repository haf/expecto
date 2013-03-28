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
            { TestAttributes.Ignore = NUnitAttr "Ignore"
              Test = NUnitAttr "Test"
              Setup = NUnitAttr "SetUp"
              TearDown = NUnitAttr "TearDown"
              FixtureSetup = NUnitAttr "TestFixtureSetUp"
              ExpectedException = NUnitAttr "ExpectedException", "ExpectedException" }
            (fun _ -> "")
