namespace Fuchu

module MbUnitTests = 
    open Fuchu
    open Fuchu.MbUnit
    open Fuchu.MbUnitTestTypes
    open NUnit.Framework
    
    [<Tests>]
    let tests() =
        "From MbUnit" =>> [
            "nothing" =>
                fun () ->
                    let test = MbUnitTestToFuchu typeof<string>
                    match test with
                    | TestList _ -> ()
                    | _ -> Assert.Fail(sprintf "Should have been TestList [], but was %A" test)

            "basic" =>> [
                let test = lazy MbUnitTestToFuchu typeof<ATestFixture>
                let result = lazy evalSilent test.Value
                yield "read tests" =>
                    fun () ->
                        Assert.AreEqual(2, result.Value.Length)
                        let testName s = sprintf "%s/%s" typeof<ATestFixture>.FullName s
                        Assert.AreEqual(testName "ATest", result.Value.[0].Name)
                        Assert.AreEqual(testName "AnotherTest", result.Value.[1].Name)
                yield "executed tests" =>
                    fun () ->
                        Assert.True(TestResult.isPassed result.Value.[0].Result)
                        Assert.True(TestResult.isException result.Value.[1].Result)
            ]

            "category" =>> [
                let test = lazy MbUnitTestToFuchu typeof<ATestFixtureWithExceptionAndTeardown>
                yield "in type" =>
                    fun _ ->
                        match test.Value with
                        | TestList t -> 
                            match Seq.toList t with
                            | [ TestLabel(listname, TestList _)] -> StringAssert.Contains("fixture category", listname)
                            | _ -> Assert.Fail (sprintf "Expected test with categories, found %A" test)
                        | _ -> Assert.Fail (sprintf "Expected test with categories, found %A" test)
                yield "in test" =>
                    fun _ ->
                        match test.Value with
                        | TestList t -> 
                            match Seq.toList t with
                            | [ TestLabel(_, TestList t)] -> 
                                match Seq.toList t with
                                | [TestLabel(name, _)] -> StringAssert.Contains("test category", name)
                                | _ -> Assert.Fail (sprintf "Expected test with categories, found %A" test)
                            | _ -> Assert.Fail (sprintf "Expected test with categories, found %A" test)
                        | _ -> Assert.Fail (sprintf "Expected test with categories, found %A" test)
            ]

            "with StaticTestFactory" =>
                fun _ ->
                    let testType = typeof<ATestFixtureWithStaticTestFactories>
                    let test = lazy MbUnitTestToFuchu testType
                    let testName = testType.Name
                    match test.Value with
                    | TestList 
                        (Seq.One (TestLabel(_, TestList 
                                                 (Seq.One (TestLabel("suite name", 
                                                              TestList (Seq.Two (TestLabel("test 1", TestCase _), TestLabel("test 2", TestCase _))))))))) -> ()
                    | _ -> Assert.Fail(sprintf "unexpected %A" test)
        ]
