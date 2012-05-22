namespace Fuchu

module MbUnit =
    open Fuchu
    open System
    open System.Reflection
    open Fuchu.Helpers
    open Fuchu.XunitHelpers

    [<RequireQualifiedAccess>]
    type private MbUnitTest =
        | TestCase of string * TestCode
        | TestSuite of string * MbUnitTest list

    let rec private MbUnitTestToFuchuTest =
        function
        | MbUnitTest.TestCase(name, code) -> Test.TestLabel(name, Test.TestCase code)
        | MbUnitTest.TestSuite(name, tests) -> Test.TestLabel(name, Test.TestList (List.map MbUnitTestToFuchuTest tests))

    let private testCaseTypeName = "MbUnit.Framework.TestCase, MbUnit"
    let private testCaseType = lazy Type.GetType testCaseTypeName
    let private testCaseExecuteProperty = lazy testCaseType.Value.GetProperty "Execute"
    let private testCaseNameProperty = lazy testCaseType.Value.GetProperty "Name"
    let private testSuiteTypeName = "MbUnit.Framework.TestSuite, MbUnit"
    let private testSuiteType = lazy Type.GetType testSuiteTypeName
    let private testSuiteNameProperty = lazy testSuiteType.Value.GetProperty "Name"
    let private testSuiteChildrenProperty = lazy testSuiteType.Value.GetProperty "Children"
    let private gallioActionTypeName = "Gallio.Common.Action, Gallio"
    let private gallioActionType = lazy Type.GetType gallioActionTypeName
    let private gallioActionInvokeMethod = lazy gallioActionType.Value.GetMethod "Invoke"

    let rec private buildMbUnitTest (o: obj) =
        let typeName = o.GetType().AssemblyQualifiedName
        if typeName.StartsWith testCaseTypeName then
            let name = testCaseNameProperty.Value.GetValue(o, null) :?> string
            let testCode = testCaseExecuteProperty.Value.GetValue(o, null)
            let invoke() = gallioActionInvokeMethod.Value.Invoke(testCode, null) |> ignore
            MbUnitTest.TestCase(name, invoke)
        elif typeName.StartsWith testSuiteTypeName then
            let name = testSuiteNameProperty.Value.GetValue(o, null) :?> string
            let children = testSuiteChildrenProperty.Value.GetValue(o, null) :?> System.Collections.IEnumerable
            let children = Seq.cast<obj> children |> Seq.map buildMbUnitTest |> Seq.toList
            MbUnitTest.TestSuite(name, children)
        else
            failwith (sprintf "%A is not a test type" o)

    let private MbUnitAttr = sprintf "MbUnit.Framework.%sAttribute"
    let private ignoreAttr = MbUnitAttr "Ignore"
    let MbUnitTestToFuchu (t: Type) =
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
            methodsWithAttrs [MbUnitAttr "Test"]
            |> Seq.filter (fun m -> not (m.HasAttribute ignoreAttr))
            |> Seq.toList
        let setupMethods = methodsWithAttrs [MbUnitAttr "SetUp"]
        let teardownMethods = methodsWithAttrs [MbUnitAttr "TearDown"]
        let fixtureSetupMethods = methodsWithAttrs [MbUnitAttr "FixtureSetUp"]
        let staticTestFactories = methodsWithAttrs [MbUnitAttr "StaticTestFactory"]

        let staticTests = 
            staticTestFactories
            |> Seq.collect (fun m -> m.Invoke(null, null) :?> System.Collections.IEnumerable |> Seq.cast<obj>)
            |> Seq.map (buildMbUnitTest >> MbUnitTestToFuchuTest)
            |> Seq.toList

        let inline invoke o (m: MethodInfo) = m.Invoke(o, null) |> ignore

        TestList [
            if testMethods.Length > 0 then
                for t in testType ->
                    t.FullName =>> [
                        let o = create t
                        let inline invoke x = invoke o x
                        Seq.iter invoke fixtureSetupMethods
                        for m in testMethods ->
                            m.Name =>
                                fun () -> 
                                    try
                                        Seq.iter invoke setupMethods
                                        invoke m
                                    finally
                                        Seq.iter invoke teardownMethods
                    ]
            if staticTests.Length > 0 then
                yield t.FullName =>> staticTests
        ]