namespace Fuchu

module XunitHelpers =
    open Fuchu
    open Fuchu.Helpers
    open System
    open System.Reflection

    let create (t: Type) =
        try
            Activator.CreateInstance t
        with e -> raise (Exception(sprintf "Couldn't instantiate test type %s" t.FullName, e))

    let methodsWithAttrs (methods: MethodInfo seq) (attr: string seq) = 
        methods
        |> Seq.filter (fun m -> Seq.exists m.HasAttribute attr)

    let methods ignoreAttr (t: Type) =
        [t]
        |> Seq.filter (fun t -> not (t.HasAttribute ignoreAttr))
        |> Seq.collect (fun _ -> t.GetMethods())

    let TestToFuchu ignoreAttr testAttr setupAttr tearDownAttr fixtureSetupAttr (testCategory: MemberInfo -> string) (t: Type) =
        let methods = methods ignoreAttr t
        let methodsWithAttrs = methodsWithAttrs methods
        let testMethods = 
            methodsWithAttrs [testAttr]
            |> Seq.filter (fun m -> not (m.HasAttribute ignoreAttr))
            |> Seq.toList
        let setupMethods = methodsWithAttrs [setupAttr]
        let teardownMethods = methodsWithAttrs [tearDownAttr]
        let fixtureSetupMethods = methodsWithAttrs [fixtureSetupAttr]

        let inline invoke o (m: MethodInfo) = m.Invoke(o, null) |> ignore

        TestList <| seq {
            if testMethods.Length > 0 then
                yield testList (t.FullName + testCategory t) <| seq {
                    let o = create t
                    let inline invoke x = invoke o x
                    Seq.iter invoke fixtureSetupMethods
                    for m in testMethods ->
                        testCase (m.Name + testCategory m) <| fun _ -> 
                            try
                                Seq.iter invoke setupMethods
                                try
                                    invoke m
                                with
                                | :? TargetInvocationException as e -> raise e.InnerException
                            finally
                                Seq.iter invoke teardownMethods
                }
        }