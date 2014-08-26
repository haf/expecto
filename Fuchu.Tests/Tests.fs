namespace Fuchu

open System

module Dummy =
    open Fuchu

    [<Tests>]
    let testA = TestLabel ("test A", TestList [])

    [<Tests>]
    let testB() = TestLabel ("test B", TestList [])

    let thisModuleType = lazy Type.GetType "Fuchu.Dummy, Fuchu.Tests"

module EmptyModule =
    let thisModuleType = lazy Type.GetType "Fuchu.EmptyModule, Fuchu.Tests"

type CustomNUnitException(msg) =
    inherit NUnit.Framework.AssertionException(msg)

module Tests =
    open Fuchu
    open Fuchu.Impl
    open System.Threading
    open System.IO
    open FSharpx
    open System.Globalization

    let (==?) actual expected = Assert.Equal("", expected, actual)

    [<Tests>]
    let tests = 
        TestList [
            testCase "basic" <| fun _ -> Assert.Equal("2+2", 4, 2+2)

            test "using computation expression" {
                Assert.Equal("2+2", 4, 2+2)
            }

            testList "sumTestResults" [
                let sumTestResultsTests = 
                    [
                        { TestRunResult.Name = ""; Result = Passed; Time = TimeSpan.FromMinutes 2. }
                        { TestRunResult.Name = ""; Result = TestResult.Error (ArgumentException()); Time = TimeSpan.FromMinutes 3. }
                        { TestRunResult.Name = ""; Result = Failed ""; Time = TimeSpan.FromMinutes 4. }
                        { TestRunResult.Name = ""; Result = Passed; Time = TimeSpan.FromMinutes 5. }
                        { TestRunResult.Name = ""; Result = Failed ""; Time = TimeSpan.FromMinutes 6. }
                        { TestRunResult.Name = ""; Result = Passed; Time = TimeSpan.FromMinutes 7. }
                    ]
                let r = lazy sumTestResults sumTestResultsTests
                yield testCase "Passed" <| fun _ -> 
                    r.Value.Passed ==? 3
                yield testCase "Failed" <| fun _ ->
                    r.Value.Failed ==? 2
                yield testCase "Exception" <| fun _ ->
                    r.Value.Errored ==? 1
                yield testCase "Time" <| fun _ ->
                    r.Value.Time ==? TimeSpan.FromMinutes 27.
            ]

            testList "TestResultCounts" [
                testList "plus" [
                    let testResultCountsSum name f =
                        testProperty name 
                            (FsCheck.Prop.forAll twoTestResultCounts.Value <|
                                fun (a,b) ->
                                    let r = a + b
                                    f a b r)
                    yield testResultCountsSum "Passed" <|
                        fun a b r -> r.Passed = a.Passed + b.Passed
                    yield testResultCountsSum "Ignored" <|
                        fun a b r -> r.Ignored = a.Ignored + b.Ignored
                    yield testResultCountsSum "Failed" <|
                        fun a b r -> r.Failed = a.Failed + b.Failed
                    yield testResultCountsSum "Errored" <|
                        fun a b r -> r.Errored = a.Errored + b.Errored
                    yield testResultCountsSum "Time" <|
                        fun a b r -> r.Time = a.Time + b.Time
                ]
                testCase "ToString" <| fun _ ->
                    let c1 = { Passed = 1; Ignored = 5; Failed = 2; Errored = 3; Time = TimeSpan.FromSeconds 20. }
                    c1.ToString() ==? "6 tests run: 1 passed, 5 ignored, 2 failed, 3 errored (00:00:20)\n"
            ]

            testList "Exception handling" [
                testCase "NUnit ignore" <| fun _ ->
                    let test() = NUnit.Framework.Assert.Ignore "a"
                    let test = TestCase test
                    match evalSilent test with
                    | [{ Result = Ignored "a" }] -> ()
                    | x -> failtestf "Expected result = Ignored, got\n %A" x

                testCase "Inherit AssertionException counts as failure" <| fun _ ->
                    let test () = raise (new CustomNUnitException("hello"))
                    let test = TestCase test
                    match evalSilent test with
                    | [{ Result = Failed (String.StartsWith "\nhello") }] -> ()
                    | x -> failtestf "Expected result = Failed, got\n %A" x

                testCase "Fuchu ignore" <| fun _ ->
                    let test () = skiptest "b"
                    let test = TestCase test
                    match evalSilent test with
                    | [{ Result = Ignored "b" }] -> ()
                    | x -> failtestf "Expected result = Ignored, got\n %A" x
            ]

            testList "Setup & teardown" [
                // just demoing how you can use a higher-order function as setup/teardown
                let withMemoryStream f () =
                    use s = new MemoryStream()
                    let r = f s
                    s.Capacity ==? 5
                    r
                yield testCase "1" (withMemoryStream (fun ms -> ms.Capacity <- 5))
                yield testCase "2" (withMemoryStream (fun ms -> ms.Capacity <- 5))
            ]

            testList "Setup & teardown 2" [
                // just demoing how you can use a higher-order function as setup/teardown
                let tests = [
                    "1", fun (ms: MemoryStream) -> ms.Capacity <- 5
                    "2", fun ms -> ms.Capacity <- 5
                ]
                let withMemoryStream f () =
                    use s = new MemoryStream()
                    let r = f s
                    s.Capacity ==? 5
                    r
                for name,test in tests ->
                    testCase name (withMemoryStream test)
            ]

            testList "Setup & teardown 3" [
                let withMemoryStream f () =
                    use ms = new MemoryStream()
                    f ms
                yield! testFixture withMemoryStream [
                    "can read", 
                        fun ms -> ms.CanRead ==? true
                    "can write",
                        fun ms -> ms.CanWrite ==? true
                ]
            ]

            testList "Test filter" [
                let tests = 
                    TestList [
                        testCase "a" ignore
                        testCase "b" ignore
                        testList "c" [
                            testCase "d" ignore
                            testCase "e" ignore
                        ]
                    ]
                yield testCase "with one testcase" <| fun _ -> 
                    let t = Test.filter ((=) "a") tests |> Test.toTestCodeList |> Seq.toList
                    t.Length ==? 1 // same as assertEqual "" 1 t.Length
                yield testCase "with nested testcase" <| fun _ ->
                    let t = Test.filter (Strings.contains "d") tests |> Test.toTestCodeList |> Seq.toList
                    t.Length ==? 1
                yield testCase "with one testlist" <| fun _ ->
                    let t = Test.filter (Strings.contains "c") tests |> Test.toTestCodeList |> Seq.toList
                    t.Length ==? 2
                yield testCase "with no results" <| fun _ ->
                    let t = Test.filter ((=) "z") tests |> Test.toTestCodeList |> Seq.toList
                    t.Length ==? 0
            ]

            testList "Timeout" [
                testCase "fail" <| fun _ ->
                    let test = TestCase(Test.timeout 10 (fun _ -> Thread.Sleep 100))
                    let result = evalSilent test |> sumTestResults
                    result.Failed ==? 1
                testCase "pass" <| fun _ ->
                    let test = TestCase(Test.timeout 1000 ignore)
                    let result = evalSilent test |> sumTestResults
                    result.Passed ==? 1
            ]

            testList "Reflection" [
                let getMember name =
                    Dummy.thisModuleType.Value.GetMember name
                    |> Array.tryFind (fun _ -> true)
                let getTest = 
                    getMember
                    >> Option.bind testFromMember
                    >> Option.bind (function TestLabel(name, _) -> Some name | _ -> None)

                yield testCase "from member" <| fun _ ->
                    getTest "testA" ==? Some "test A"
                yield testCase"from function" <| fun _ ->
                    getTest "testB" ==? Some "test B"
                yield testCase"from type" <| fun _ ->
                    match testFromType Dummy.thisModuleType.Value with
                    | Some (TestList (
                                Seq.Two (
                                    TestLabel("test B", TestList _), 
                                    TestLabel("test A", TestList _)))) -> ()
                    | x -> failtestf "TestList expected, found %A" x
                yield testCase "from empty type" <| fun _ ->
                    let test = testFromType EmptyModule.thisModuleType.Value
                    Assert.None("", test)
            ]

            testList "parse args" [
                testCase "default" <|
                    fun _ ->
                        let opts = parseArgs [||]
                        opts.Parallel ==? false

                testCase "parallel" <|
                    fun _ ->
                        let opts = parseArgs [|"/m"|]
                        opts.Parallel ==? true
            ]

            testList "transformations" [
                testCase "multiple cultures" <| fun _ ->

                    let withCulture culture f x = 
                        let c = Thread.CurrentThread.CurrentCulture
                        try
                            Thread.CurrentThread.CurrentCulture <- culture
                            f x
                        finally
                            Thread.CurrentThread.CurrentCulture <- c

                    let testWithCultures (cultures: #seq<CultureInfo>) = 
                        Test.replaceTestCode <| fun name test ->
                            testList name [
                                for c in cultures ->
                                    testCase c.Name (withCulture c test)
                            ]

                    let atest = test "parse" {
                        Single.Parse("123,33") ==? 123.33f
                    }

                    let cultures = 
                        ["en-US"; "es-AR"; "fr-FR"]
                        |> List.map CultureInfo.GetCultureInfo

                    let culturizedTests = testWithCultures cultures atest

                    let results = 
                        evalSilent culturizedTests
                        |> Seq.map (fun r -> r.Name, r.Result)
                        |> Map.ofSeq

                    Assert.Equal("results count", 3, results.Count)
                    let inline assertTrue msg x = Assert.Equal(msg, true, x)
                    assertTrue "parse en-US fails" (TestResult.isFailed results.["parse/en-US"])
                    assertTrue "parse es-AR passes" (TestResult.isPassed results.["parse/es-AR"])
                    assertTrue "parse fr-FR passes" (TestResult.isPassed results.["parse/fr-FR"])
            ]

            testList "assertions" [
                testList "NotEqual" [
                    testCase "pass" <| fun _ ->
                        Assert.NotEqual("should be different", "", "monkey")

                    testCase "fail" <| fun _ ->
                        let test () = Assert.NotEqual("should fail", "", "")
                        assertTestFails test
                ]

                testList "raise" [
                    testCase "pass" <| fun _ ->
                        Assert.Raise("", typeof<ArgumentNullException>, fun _ -> nullArg "")

                    testCase "fail with incorrect exception" <| fun _ ->
                        let test () = Assert.Raise("", typeof<ArgumentException>, fun _ -> nullArg "")
                        assertTestFails test
                    
                    testCase "fail with no exception" <| fun _ ->
                        let test () = Assert.Raise("", typeof<ArgumentNullException>, ignore)
                        assertTestFails test
                ]

                testList "string contain" [
                    testCase "pass" <| fun _ ->
                        Assert.StringContains("", "hello", "hello world")

                    testCase "fail" <| fun _ ->
                        let test () = Assert.StringContains("", "a", "hello world")
                        assertTestFails test
                ]
            ]

            testList "computation expression" [
                let testNormal a =
                    testCase "" <| fun _ ->
                        if a < 0
                            then failtest "negative"
                        if a > 5
                            then failwith "over 5"
                let testCompExp a = 
                    test "" {
                        if a < 0
                            then failtest "negative"
                        if a > 5
                            then failwith "over 5"
                    }
                for c in [-5; 1; 6] ->
                    testCase (sprintf "compare comp.exp. and normal with value %d" c) <| fun _ ->
                        let normal = evalSilent <| testNormal c
                        let compexp = evalSilent <| testCompExp c
                        let normalTag = TestResult.tag normal.[0].Result
                        let compexpTag = TestResult.tag compexp.[0].Result
                        Assert.Equal("result", normalTag, compexpTag)
            ]
        ]
