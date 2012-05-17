namespace Fuchu

module NUnit =
    open System
    open System.Reflection
    
    let private NUnitAttr = sprintf "NUnit.Framework.%sAttribute"
    let NUnitTestToFuchu (t: Type) =
        let ignoreAttr = NUnitAttr "Ignore"
        let testType = 
            [t]
            |> Seq.filter (fun t -> t.HasAttribute (NUnitAttr "TestFixture"))
            |> Seq.filter (fun t -> not (t.HasAttribute ignoreAttr))
        let methods = 
            testType
            |> Seq.collect (fun _ -> t.GetMethods())
            |> Seq.toList
        let inline methodsWithAttrs (attr: string seq) = 
            methods
            |> Seq.filter (fun m -> Seq.exists m.HasAttribute attr)
        let testMethods = 
            methodsWithAttrs [NUnitAttr "Test"]
            |> Seq.filter (fun m -> not (m.HasAttribute ignoreAttr))
        let setupMethods = methodsWithAttrs [NUnitAttr "SetUp"]
        let teardownMethods = methodsWithAttrs [NUnitAttr "TearDown"]
        let fixtureSetupMethods = methodsWithAttrs [NUnitAttr "TestFixtureSetUp"]

        let inline invoke o (m: MethodInfo) = m.Invoke(o, null) |> ignore

        TestList [
            for t in testType ->
                t.FullName =>> [
                    let o = Activator.CreateInstance t
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
        ]
