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

    let nonIgnoredMethods ignoreAttr (t: Type) =
        [t]
#if DNXCORE50
        |> Seq.filter (fun t -> not (t.GetTypeInfo().HasAttribute ignoreAttr))
#else
        |> Seq.filter (fun t -> not (t.HasAttribute ignoreAttr))
#endif
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

    let inline invoke (o: obj) (m: MethodInfo) = m.Invoke(o, null) |> ignore

    type TestMethod = {
        Name: string
        Invoke: obj -> unit
        ExpectedException: string option
    }

    let getTestMethods (testCategory: MethodInfo -> string) (attr: TestAttributes) (testType: Type) =
        let methods = nonIgnoredMethods attr.Ignore testType
        let methodsWithAttrs = methodsWithAttrs methods
        methodsWithAttrs [attr.Test]
        |> Seq.filter (fun m -> not (m.HasAttribute attr.Ignore))
        |> Seq.filter (fun m -> m.GetParameters().Length = 0)
        |> Seq.map (fun m -> { TestMethod.Name = m.Name + testCategory m
                               ExpectedException = expectedException attr.ExpectedException m
                               Invoke = fun (o: obj) -> invoke o m })

    let TestToFuchu (attr: TestAttributes) (testCategory: Type -> string) (testType: Type) (testMethods: TestMethod seq) =
#if DNXCORE50
        if testType.GetTypeInfo().IsAbstract
#else
        if testType.IsAbstract
#endif
            then TestList []
            else
                let methods = nonIgnoredMethods attr.Ignore testType
                let methodsWithAttrs = methodsWithAttrs methods
                let setupMethods = methodsWithAttrs [attr.Setup]
                let teardownMethods = methodsWithAttrs [attr.TearDown]
                let fixtureSetupMethods = methodsWithAttrs [attr.FixtureSetup]

                TestList <| seq {
                    if Seq.length testMethods > 0 then
                        yield testList (testType.FullName + testCategory testType) <| seq {
                            let testInstance = create testType
                            let inline invoke metod = invoke testInstance metod
                            Seq.iter invoke fixtureSetupMethods
                            for tm in testMethods ->
                                test tm.Name {
                                    try
                                        Seq.iter invoke setupMethods
                                        try
                                            tm.Invoke testInstance
                                            match tm.ExpectedException with
                                            | Some expectedExc ->
                                                failtestf "Expected exception '%s' but no exception was thrown" expectedExc
                                            | None -> ()
                                        with
                                        | :? TargetInvocationException as e -> 
                                            match tm.ExpectedException with
                                            | Some expectedExc ->
                                                let actualExc = 
                                                    if e.InnerException <> null
                                                        then e.InnerException.GetType().AssemblyQualifiedName
                                                        else e.GetType().AssemblyQualifiedName
                                                if expectedExc <> actualExc
                                                    then failtestf "Expected exception '%s', got '%s'" expectedExc actualExc
                                            | None ->
                                                raise e.InnerException
                                    finally
                                        Seq.iter invoke teardownMethods
                                }
                        }
        }