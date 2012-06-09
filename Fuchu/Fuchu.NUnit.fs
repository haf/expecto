namespace Fuchu

module NUnit =
    open Fuchu
    open System
    open System.Reflection
    open Fuchu.Helpers
    open Fuchu.XunitHelpers
    
    let private NUnitAttr = sprintf "NUnit.Framework.%sAttribute"
    let private ignoreAttr = NUnitAttr "Ignore"

    let NUnitTestToFuchu (t: Type) =
        let testType = 
            [t]
            |> Seq.filter (fun t -> not (t.HasAttribute ignoreAttr))
            |> Seq.toList
        let methods = 
            testType
            |> Seq.collect (fun _ -> t.GetMethods())
            |> Seq.toList
        let inline methodsWithAttrs (attr: string seq) = 
            methods
            |> Seq.filter (fun m -> Seq.exists m.HasAttribute attr)
            |> Seq.toList
        let testMethods = 
            methodsWithAttrs [NUnitAttr "Test"]
            |> Seq.filter (fun m -> not (m.HasAttribute ignoreAttr))
            |> Seq.toList
        let setupMethods = methodsWithAttrs [NUnitAttr "SetUp"]
        let teardownMethods = methodsWithAttrs [NUnitAttr "TearDown"]
        let fixtureSetupMethods = methodsWithAttrs [NUnitAttr "TestFixtureSetUp"]

        let inline invoke o (m: MethodInfo) = m.Invoke(o, null) |> ignore

        TestList (seq {
            yield buildTestSuite testMethods fixtureSetupMethods setupMethods teardownMethods t (fun _ -> "") invoke
        })
