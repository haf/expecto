namespace Fuchu

module NUnitTests = 
    open Fuchu
    open Fuchu.NUnit
    open Fuchu.NUnitTestTypes
    open NUnit.Framework

    [<Tests>]
    let tests = 
        "From NUnit" =>> [
            "nothing" =>
                fun () ->
                    let test = NUnitTestToFuchu typeof<string>
                    match test with
                    | TestList Seq.Empty -> ()
                    | _ -> Assert.Fail(sprintf "Should have been TestList [], but was %A" test)

            "basic" =>> [
                let result = lazy evalSilent (NUnitTestToFuchu typeof<ATestFixture>)
                yield "read tests" =>
                    fun () ->
                        Assert.AreEqual(2, result.Value.Length)
                        let testName s = sprintf "%s/%s" typeof<ATestFixture>.FullName s
                        Assert.AreEqual(testName "ATest", result.Value.[0].Name)
                        Assert.AreEqual(testName "AnotherTest", result.Value.[1].Name)
                yield "executed tests" =>
                    fun () ->
                        Assert.True(TestResult.isPassed result.Value.[0].Result)
                        Assert.True(TestResult.isFailed result.Value.[1].Result)
            ]

            "with setup" =>
                fun () ->
                    let test = NUnitTestToFuchu typeof<ATestFixtureWithSetup>
                    Assert.False(ATestFixtureWithSetup.TearDownCalled, "TearDown was called")
                    let result = evalSilent test
                    Assert.AreEqual(1, result.Length)
                    Assert.True(TestResult.isPassed result.[0].Result, "Test not passed")
                    Assert.True(ATestFixtureWithSetup.TearDownCalled, "TearDown was not called")

            "with teardown and exception in test" =>
                fun () ->
                    let test = NUnitTestToFuchu typeof<ATestFixtureWithExceptionAndTeardown>
                    Assert.False(ATestFixtureWithExceptionAndTeardown.TearDownCalled, "TearDown was called")
                    let result = evalSilent test
                    Assert.AreEqual(1, result.Length)
                    Assert.True(TestResult.isException result.[0].Result, "Test not failed")
                    Assert.True(ATestFixtureWithExceptionAndTeardown.TearDownCalled, "TearDown was not called")
        ]
