namespace Fuchu

module MbUnitTests = 
    open Fuchu
    open Fuchu.Impl
    open Fuchu.MbUnit
    open Fuchu.MbUnitTestTypes
    
    [<Tests>]
    let tests =
        "From MbUnit" =>> [
            "nothing" =>
                fun () ->
                    let test = MbUnitTestToFuchu typeof<string>
                    match test with
                    | TestList (Seq.One (TestList Seq.Empty)) -> ()
                    | _ -> failtestf "Should have been TestList [], but was %A" test

            "basic" =>> [
                let result = lazy evalSilent (MbUnitTestToFuchu typeof<ATestFixture>)
                yield "read tests" =>
                    fun () ->
                        result.Value.Length =? 2
                        let testName s = sprintf "%s/%s" typeof<ATestFixture>.FullName s
                        result.Value.[0].Name =? testName "ATest"
                        result.Value.[1].Name =? testName "AnotherTest"

                yield "executed tests" =>
                    fun () ->
                        TestResult.isPassed result.Value.[0].Result =? true
                        TestResult.isException result.Value.[1].Result =? true
            ]

            "category" =>> [
                let test = lazy MbUnitTestToFuchu typeof<ATestFixtureWithExceptionAndTeardown>
                yield "in type" =>
                    fun _ ->
                        match test.Value with
                        | TestList 
                            (Seq.One (TestList 
                                        (Seq.One (TestLabel(String.Contains "fixture category", TestList _))))) -> ()
                        | _ -> failtestf "Expected test with categories, found %A" test.Value

                yield "in test" =>
                    fun _ ->
                        match test.Value with
                        | TestList 
                            (Seq.One (TestList 
                                        (Seq.One (TestLabel(_, TestList 
                                                                (Seq.One (TestLabel(String.Contains "test category", _)))))))) -> ()
                        | _ -> failtestf "Expected test with categories, found %A" test.Value
            ]

            "with StaticTestFactory" =>
                fun _ ->
                    let testType = typeof<ATestFixtureWithStaticTestFactories>
                    let test = MbUnitTestToFuchu testType
                    let testName = testType.Name
                    match test with
                    | TestList 
                        (Seq.Two (TestList _, 
                            TestLabel(_, TestList 
                                (Seq.One (TestLabel("suite name", TestList 
                                            (Seq.Two (TestLabel("test 1", TestCase _), TestLabel("test 2", TestCase _))))))))) -> ()
                    | _ -> failtestf "unexpected %A" test
        ]
