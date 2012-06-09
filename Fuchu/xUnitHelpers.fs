namespace Fuchu

module XunitHelpers =
    open System
    open System.Reflection

    let create (t: Type) =
        try
            Activator.CreateInstance t
        with e -> raise (Exception(sprintf "Couldn't instantiate test type %s" t.FullName, e))

    let buildTestSuite (testMethods: MethodInfo list) (fixtureSetupMethods: MethodInfo list) (setupMethods: MethodInfo list) (teardownMethods: MethodInfo list) (t: Type) (testCategory: MemberInfo -> string) invoke =
        if testMethods.Length > 0 then
            t.FullName + testCategory t =>> seq {
                let o = create t
                let inline invoke x = invoke o x
                Seq.iter invoke fixtureSetupMethods
                for m in testMethods ->
                    m.Name + testCategory m =>
                        fun () -> 
                            try
                                Seq.iter invoke setupMethods
                                try
                                    invoke m
                                with
                                | :? TargetInvocationException as e -> raise e.InnerException
                            finally
                                Seq.iter invoke teardownMethods
            }
        else TestList []
