namespace Fuchu

module NUnitTests = 
    open Fuchu
    open Fuchu.Impl
    open Fuchu.NUnit
    open Fuchu.NUnitTestTypes

    [<Tests>]
    let tests = 
        testList "From NUnit" [
            testCase "nothing" <| fun _ ->
                let test = NUnitTestToFuchu typeof<string>
                match test with
                | TestList Seq.Empty -> ()
                | _ -> failtestf "Should have been TestList [], but was %A" test

            testList "basic" [
                let result = lazy evalSilent (NUnitTestToFuchu typeof<ATestFixture>)
                yield testCase "read tests" <| fun _ ->
                    Assert.Equal("test list length", 3, result.Value.Length)
                    let testName s = sprintf "%s/%s" typeof<ATestFixture>.FullName s
                    Assert.Equal("first test name", testName "ATest", result.Value.[0].Name)
                    Assert.Equal("second test name", testName "AnotherTest", result.Value.[1].Name)

                yield testCase "executed tests" <| fun _ ->
                    if not (TestResult.isPassed result.Value.[0].Result) 
                        then failtestf "Expected first test to be passed, actual %A" result.Value.[0].Result
                    if not (TestResult.isFailed result.Value.[1].Result) 
                        then failtestf "Expected second test to be failed, actual %A" result.Value.[1].Result
                    if not (TestResult.isPassed result.Value.[2].Result) 
                        then failtestf "Expected third test to be passed, actual %A" result.Value.[2].Result
            ]

            testCase "with setup" <| fun _ ->
                let test = NUnitTestToFuchu typeof<ATestFixtureWithSetup>
                if ATestFixtureWithSetup.TearDownCalled 
                    then failtest "TearDown was called"
                let result = evalSilent test
                Assert.Equal("test list length", 1, result.Length)
                if not (TestResult.isPassed result.[0].Result)
                    then failtest "test not passed"
                if not ATestFixtureWithSetup.TearDownCalled
                    then failtest "TearDown was not called"

            testCase "with teardown and exception in test" <| fun _ ->
                let test = NUnitTestToFuchu typeof<ATestFixtureWithExceptionAndTeardown>
                if ATestFixtureWithExceptionAndTeardown.TearDownCalled
                    then failtest "TearDown was called"
                let result = evalSilent test
                Assert.Equal("test list length", 1, result.Length)
                if not (TestResult.isException result.[0].Result)
                    then failtest "Test not failed"
                if not ATestFixtureWithExceptionAndTeardown.TearDownCalled
                    then failtest "TearDown was not called"
        ]
