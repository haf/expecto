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

    [<Tests>]
    let tests = 
        TestList [
            "basic" => 
                fun () -> 2+2 =? 4
            "sumTestResults" =>> [
                let sumTestResultsTests = 
                    [
                        { TestRunResult.Name = ""; Result = Passed; Time = TimeSpan.FromMinutes 2. }
                        { TestRunResult.Name = ""; Result = Error (ArgumentException()); Time = TimeSpan.FromMinutes 3. }
                        { TestRunResult.Name = ""; Result = Failed ""; Time = TimeSpan.FromMinutes 4. }
                        { TestRunResult.Name = ""; Result = Passed; Time = TimeSpan.FromMinutes 5. }
                        { TestRunResult.Name = ""; Result = Failed ""; Time = TimeSpan.FromMinutes 6. }
                        { TestRunResult.Name = ""; Result = Passed; Time = TimeSpan.FromMinutes 7. }
                    ]
                let r = lazy sumTestResults sumTestResultsTests
                yield "Passed" =>
                    fun () -> r.Value.Passed =? 3
                yield "Failed" =>
                    fun () -> r.Value.Failed =? 2
                yield "Exception" =>
                    fun () -> r.Value.Errored =? 1
                yield "Time" =>
                    fun () -> r.Value.Time =? TimeSpan.FromMinutes 27.
            ]
            "TestResultCounts" =>> [
                "plus" =>> [
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
                "ToString" => 
                    let c1 = { Passed = 1; Ignored = 5; Failed = 2; Errored = 3; Time = TimeSpan.FromSeconds 20. }
                    fun () -> c1.ToString() =? "6 tests run: 1 passed, 5 ignored, 2 failed, 3 errored (00:00:20)\n"
            ]

            "Exception handling" =>> [
                "NUnit ignore" => 
                    fun () ->
                        let test() = NUnit.Framework.Assert.Ignore "a"
                        let test = TestCase test
                        match evalSilent test with
                        | [{ Result = Ignored "a" }] -> ()
                        | x -> failtestf "Wrong test evaluation\n %A" x

                "Inherit AssertionException counts as failure" =>
                    fun _ ->
                        let test () = raise (new CustomNUnitException("hello"))
                        let test = TestCase test
                        match evalSilent test with
                        | [{ Result = Failed (String.StartsWith "\nhello") }] -> ()
                        | x -> failtestf "Wrong test evaluation\n %A" x
            ]

            "Setup & teardown" =>> [
                // just demoing how you can use a higher-order function as setup/teardown
                let withMemoryStream f () =
                    use s = new MemoryStream()
                    let r = f s
                    s.Capacity =? 5
                    r
                yield "1" => withMemoryStream (fun ms -> ms.Capacity <- 5)
                yield "2" => withMemoryStream (fun ms -> ms.Capacity <- 5)
            ]
            "Setup & teardown 2" =>> [
                // just demoing how you can use a higher-order function as setup/teardown
                let tests = [
                    "1", fun (ms: MemoryStream) -> ms.Capacity <- 5
                    "2", fun ms -> ms.Capacity <- 5
                ]
                let withMemoryStream f () =
                    use s = new MemoryStream()
                    let r = f s
                    s.Capacity =? 5
                    r
                for name,test in tests ->
                    name => withMemoryStream test
            ]
            "Setup & teardown 3" =>> [
                let withMemoryStream f () =
                    use ms = new MemoryStream()
                    f ms
                yield! withMemoryStream +> [
                    "can read" ==> 
                        fun ms -> 
                            if not (ms.CanRead) then failtest "Can't read!"
                    "can write" ==>
                        fun ms -> ms.CanWrite =? true
                ]
                // alt syntax
                yield! testFixture withMemoryStream [
                    "can read", 
                        fun ms -> ms.CanRead =? true
                    "can write",
                        fun ms -> ms.CanWrite =? true
                ]
            ]
            "Test filter" =>> [
                let tests = 
                    TestList [
                        "a" => ignore
                        "b" => ignore
                        "c" =>> [
                            "d" => ignore
                            "e" => ignore
                        ]
                    ]
                yield "with one testcase" =>
                    fun () -> 
                        let t = Test.filter ((=) "a") tests |> Test.toTestCodeList |> Seq.toList
                        t.Length =? 1 // same as assertEqual "" 1 t.Length
                yield "with nested testcase" =>
                    fun () -> 
                        let t = Test.filter (Strings.contains "d") tests |> Test.toTestCodeList |> Seq.toList
                        t.Length =? 1
                yield "with one testlist" =>
                    fun () -> 
                        let t = Test.filter (Strings.contains "c") tests |> Test.toTestCodeList |> Seq.toList
                        t.Length =? 2
                yield "with no results" =>
                    fun () -> 
                        let t = Test.filter ((=) "z") tests |> Test.toTestCodeList |> Seq.toList
                        t.Length =? 0
            ]
            "Timeout" =>> [
                "fail" =>
                    fun _ ->
                        let test = TestCase(Test.timeout 10 (fun _ -> Thread.Sleep 100))
                        let result = evalSilent test |> sumTestResults
                        result.Failed =? 1
                "pass" =>
                    fun _ ->
                        let test = TestCase(Test.timeout 1000 ignore)
                        let result = evalSilent test |> sumTestResults
                        result.Passed =? 1
            ]
            "Reflection" =>> [                
                let getMember name =
                    Dummy.thisModuleType.Value.GetMember name
                    |> Array.tryFind (fun _ -> true)
                let getTest = 
                    getMember
                    >> Option.bind testFromMember
                    >> Option.bind (function TestLabel(name, _) -> Some name | _ -> None)

                yield "from member" => 
                    fun _ ->
                        getTest "testA" =? Some "test A"
                yield "from function" =>
                    fun _ ->
                        getTest "testB" =? Some "test B"
                yield "from type" =>
                    fun _ ->
                        match testFromType Dummy.thisModuleType.Value with
                        | Some (TestList (
                                    Seq.Two (
                                        TestLabel("test B", TestList _), 
                                        TestLabel("test A", TestList _)))) -> ()
                        | x -> failtestf "TestList expected, found %A" x
                yield "from empty type" =>
                    fun _ ->
                        let test = testFromType EmptyModule.thisModuleType.Value
                        if not test.IsNone
                            then failtestf "Expected None, was %A" test
            ]

            testList "parse args" [
                testCase "default" <|
                    fun _ ->
                        let opts = parseArgs [||]
                        opts.Parallel =? false

                testCase "parallel" <|
                    fun _ ->
                        let opts = parseArgs [|"/m"|]
                        opts.Parallel =? true
            ]
        ]

