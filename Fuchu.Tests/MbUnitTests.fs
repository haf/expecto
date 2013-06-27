namespace Fuchu

module MbUnitTests = 
    open Fuchu
    open Fuchu.Impl
    open Fuchu.MbUnit
    open Fuchu.MbUnitTestTypes
    
    [<Tests>]
    let tests =
        testList "From MbUnit" [
            testCase "nothing" <| fun _ ->
                let test = MbUnitTestToFuchu typeof<string>
                match test with
                | TestList (Seq.One (TestList Seq.Empty)) -> ()
                | _ -> failtestf "Should have been TestList [], but was %A" test

            testList "basic" [
                let result = lazy evalSilent (MbUnitTestToFuchu typeof<ATestFixture>)
                yield testCase "read tests" <| fun _ ->
                    Assert.Equal("length", 4, result.Value.Length)
                    let testName s = sprintf "%s/%s" typeof<ATestFixture>.FullName s
                    Assert.Equal("first test name", testName "ATest", result.Value.[0].Name)
                    Assert.Equal("second test name", testName "AnotherTest", result.Value.[1].Name)
                    Assert.Equal("third test name", testName "ATestWithExpectedException", result.Value.[2].Name)

                yield testCase "executed tests" <| fun _ ->
                    Assert.Equal("first test passed", true, TestResult.isPassed result.Value.[0].Result)
                    Assert.Equal("second test errored", true, TestResult.isException result.Value.[1].Result)
                    Assert.Equal("third test passed", true, TestResult.isPassed result.Value.[2].Result)
                    Assert.Equal("third test failed", true, TestResult.isFailed result.Value.[3].Result)
                    
            ]

            testList "category" [
                let test = lazy MbUnitTestToFuchu typeof<ATestFixtureWithExceptionAndTeardown>
                yield testCase "in type" <| fun _ ->
                    match test.Value with
                    | TestList 
                        (Seq.One (TestList 
                                    (Seq.One (TestLabel(String.Contains "fixture category", TestList _))))) -> ()
                    | _ -> failtestf "Expected test with categories, found %A" test.Value

                yield testCase "in test" <| fun _ ->
                    match test.Value with
                    | TestList 
                        (Seq.One (TestList 
                                    (Seq.One (TestLabel(_, TestList 
                                                            (Seq.One (TestLabel(String.Contains "test category", _)))))))) -> ()
                    | _ -> failtestf "Expected test with categories, found %A" test.Value
            ]

            testCase "with StaticTestFactory in TestSuite" <| fun _ ->
                let testType = typeof<ATestFixtureWithStaticTestFactories>
                let test = MbUnitTestToFuchu testType
                match test with
                | TestList 
                    (Seq.Two (TestList _, 
                        TestLabel(_, TestList 
                            (Seq.One (TestLabel("suite name", TestList 
                                        (Seq.Two (TestLabel("test 1", TestCase _), TestLabel("test 2", TestCase _))))))))) -> ()
                | _ -> failtestf "unexpected %A" test

            testCase "with StaticTestFactory in list of TestCases" <| fun _ ->
                let testType = typeof<ATestFixtureWithStaticTestFactories2>
                let test = MbUnitTestToFuchu testType
                match test with
                | TestList 
                    (Seq.Two (TestList _,
                        TestLabel(_, TestList
                            (Seq.Two (TestLabel("test 1", TestCase _), TestLabel("test 2", TestCase _)))))) -> ()
                | _ -> failtestf "unexpected %A" test
                
            testCase "row test" <| fun _ ->
                let testType = typeof<ATestFixtureWithRow>
                let test = MbUnitTestToFuchu testType
                match test with
                | TestList
                    (Seq.One (TestList 
                                (Seq.One(TestLabel(String.Contains "ATestFixtureWithRow", 
                                            TestList
                                                (Seq.Two (
                                                    TestLabel("DivTest(1000,10,100)", TestCase _), 
                                                    TestLabel("DivTest(4195835,3145729,1.3338196)", TestCase _)))))))) -> ()
                | _ -> failtestf "unexpected %A" test
        ]

    type MbUnitTestsFromFuchu() =
        [<MbUnit.Framework.StaticTestFactory>]
        static member Tests() =
            [Fuchu.MbUnit.FuchuTestToMbUnit "" Fuchu.Tests.tests]

    [<Tests>]            
    let testsToMbUnit =
        test "To MbUnit and back" {
            let fuchuTests = Fuchu.MbUnit.MbUnitTestToFuchu typeof<MbUnitTestsFromFuchu>
            let result = evalSilent fuchuTests
            for r in result do
                //printfn "%s passed" r.Name
                Assert.Equal(sprintf "%s passed" r.Name, TestResult.Passed, r.Result)
        }