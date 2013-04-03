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

    let propertyValue propertyName o =
        let prop = o.GetType().GetProperty propertyName
        prop.GetValue(o, null)

    let expectedException (expectedExceptionAttr, exPropName) (m: MethodInfo) = 
        m.GetAttributes expectedExceptionAttr
        |> Seq.map (fun a ->
                        let v = propertyValue exPropName a :?> Type
                        v.AssemblyQualifiedName)
        |> Seq.tryFind (fun _ -> true)

    type TestAttributes = {
        Ignore: string
        Test: string
        Setup: string
        TearDown: string
        FixtureSetup: string
        ExpectedException: string * string
    }


    let TestToFuchu (attr: TestAttributes) (testCategory: MemberInfo -> string) (t: Type) =
        let methods = methods attr.Ignore t
        let methodsWithAttrs = methodsWithAttrs methods
        let testMethods = 
            methodsWithAttrs [attr.Test]
            |> Seq.filter (fun m -> not (m.HasAttribute attr.Ignore))
            |> Seq.toList
        let setupMethods = methodsWithAttrs [attr.Setup]
        let teardownMethods = methodsWithAttrs [attr.TearDown]
        let fixtureSetupMethods = methodsWithAttrs [attr.FixtureSetup]
        let expectedException = expectedException attr.ExpectedException

        let inline invoke o (m: MethodInfo) = m.Invoke(o, null) |> ignore

        TestList <| seq {
            if testMethods.Length > 0 then
                yield testList (t.FullName + testCategory t) <| seq {
                    let o = create t
                    let inline invoke x = invoke o x
                    Seq.iter invoke fixtureSetupMethods
                    for m in testMethods ->
                        test (m.Name + testCategory m) {
                            try
                                Seq.iter invoke setupMethods
                                let expectedException = expectedException m 
                                try
                                    let r = invoke m
                                    match expectedException with
                                    | Some expectedExc ->
                                        failtestf "Expected exception '%s' but no exception was thrown" expectedExc
                                    | None -> r
                                with
                                | :? TargetInvocationException as e -> 
                                    match expectedException with
                                    | Some expectedExc ->
                                        let innerExc = e.InnerException.GetType().AssemblyQualifiedName
                                        if expectedExc <> innerExc
                                            then failtestf "Expected exception '%s', got '%s'" expectedExc innerExc
                                    | None ->
                                        raise e.InnerException
                            finally
                                Seq.iter invoke teardownMethods
                        }
                }
        }