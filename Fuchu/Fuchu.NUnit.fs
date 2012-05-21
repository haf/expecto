namespace Fuchu

module NUnit =
    open Fuchu
    open System
    open System.Reflection
    open Fuchu.Helpers
    
    let private NUnitAttr = sprintf "NUnit.Framework.%sAttribute"
    let NUnitTestToFuchu (t: Type) =
        let ignoreAttr = NUnitAttr "Ignore"
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
        let testMethods = 
            methodsWithAttrs [NUnitAttr "Test"]
            |> Seq.filter (fun m -> not (m.HasAttribute ignoreAttr))
            |> Seq.toList
        let setupMethods = methodsWithAttrs [NUnitAttr "SetUp"]
        let teardownMethods = methodsWithAttrs [NUnitAttr "TearDown"]
        let fixtureSetupMethods = methodsWithAttrs [NUnitAttr "TestFixtureSetUp"]

        let inline invoke o (m: MethodInfo) = m.Invoke(o, null) |> ignore

        let create (t: Type) =
            try
                Activator.CreateInstance t
            with e -> raise (Exception(sprintf "Couldn't instantiate test type %s" t.FullName, e))

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
        ]
