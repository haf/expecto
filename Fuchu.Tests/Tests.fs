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

module Tests =
    open Fuchu
    open System.Threading
    open System.IO
    open FSharpx
    open NUnit.Framework

    [<Tests>]
    let tests = 
        TestList [
            "basic" => 
                fun () -> Assert.AreEqual(4, 2+2)
            "sumTestResults" =>> [
                let sumTestResultsTests = 
                    [
                        { TestRunResult.Name = ""; Result = Passed; Time = TimeSpan.FromMinutes 2. }
                        { TestRunResult.Name = ""; Result = Exception (ArgumentException()); Time = TimeSpan.FromMinutes 3. }
                        { TestRunResult.Name = ""; Result = Failed ""; Time = TimeSpan.FromMinutes 4. }
                        { TestRunResult.Name = ""; Result = Passed; Time = TimeSpan.FromMinutes 5. }
                        { TestRunResult.Name = ""; Result = Failed ""; Time = TimeSpan.FromMinutes 6. }
                        { TestRunResult.Name = ""; Result = Passed; Time = TimeSpan.FromMinutes 7. }
                    ]
                let r = lazy sumTestResults sumTestResultsTests
                yield "Passed" =>
                    fun () -> Assert.AreEqual(3, r.Value.Passed)
                yield "Failed" =>
                    fun () -> Assert.AreEqual(2, r.Value.Failed)
                yield "Exception" =>
                    fun () -> Assert.AreEqual(1, r.Value.Errored)
                yield "Time" =>
                    fun () -> Assert.AreEqual(TimeSpan.FromMinutes 27., r.Value.Time)
            ]
            "TestResultCounts" =>> [
                let c1 = { Passed = 1; Ignored = 5; Failed = 2; Errored = 3; Time = TimeSpan.FromSeconds 20. }
                yield "plus" =>> [
                    let c2 = { Passed = 2; Ignored = 6; Failed = 3; Errored = 4; Time = TimeSpan.FromSeconds 25. }
                    let r = c1 + c2
                    yield "Passed" => fun () -> Assert.AreEqual(3, r.Passed)
                    yield "Ignored" => fun () -> Assert.AreEqual(11, r.Ignored)
                    yield "Failed" => fun () -> Assert.AreEqual(5, r.Failed)
                    yield "Errored" => fun () -> Assert.AreEqual(7, r.Errored)
                    yield "Time" => fun () -> Assert.AreEqual(TimeSpan.FromSeconds 45., r.Time)
                ]
                yield "ToString" => 
                    fun () -> Assert.AreEqual("6 tests run: 1 passed, 5 ignored, 2 failed, 3 errored (00:00:20)\n", c1.ToString())
            ]
            "Exception handling" =>> [
                "NUnit ignore" => 
                    fun () ->
                        let test() = Assert.Ignore "a"
                        let test = TestCase test
                        match evalSilent test with
                        | [{ Result = Ignored "a" }] -> ()
                        | x -> Assert.Fail "Wrong test evaluation"
            ]
            "Setup & teardown" =>> [
                let withMemoryStream f () =
                    use s = new MemoryStream()
                    let r = f s
                    Assert.AreEqual(5, s.Capacity)
                    r
                yield "1" => withMemoryStream (fun ms -> ms.Capacity <- 5)
                yield "2" => withMemoryStream (fun ms -> ms.Capacity <- 5)
            ]
            "Setup & teardown 2" =>> [
                let tests = [
                    "1", fun (ms: MemoryStream) -> ms.Capacity <- 5
                    "2", fun ms -> ms.Capacity <- 5
                ]
                let withMemoryStream f () =
                    use s = new MemoryStream()
                    let r = f s
                    Assert.AreEqual(5, s.Capacity)
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
                        fun ms -> Assert.True(ms.CanRead)
                    "can write" ==>
                        fun ms -> Assert.True(ms.CanWrite)
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
                        Assert.AreEqual(1, t.Length)
                yield "with nested testcase" =>
                    fun () -> 
                        let t = Test.filter (Strings.contains "d") tests |> Test.toTestCodeList |> Seq.toList
                        Assert.AreEqual(1, t.Length)
                yield "with one testlist" =>
                    fun () -> 
                        let t = Test.filter (Strings.contains "c") tests |> Test.toTestCodeList |> Seq.toList
                        Assert.AreEqual(2, t.Length)
                yield "with no results" =>
                    fun () -> 
                        let t = Test.filter ((=) "z") tests |> Test.toTestCodeList |> Seq.toList
                        Assert.AreEqual(0, t.Length)
            ]
            "Timeout" =>> [
                "fail" =>
                    fun _ ->
                        let test = TestCase(Test.timeout 10 (fun _ -> Thread.Sleep 100))
                        let result = evalSilent test |> sumTestResults
                        Assert.AreEqual(1, result.Failed)
                "pass" =>
                    fun _ ->
                        let test = TestCase(Test.timeout 1000 ignore)
                        let result = evalSilent test |> sumTestResults
                        Assert.AreEqual(1, result.Passed)
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
                        match getTest "testA" with
                        | Some name -> Assert.AreEqual("test A", name)
                        | _ -> Assert.Fail "no test found"
                yield "from function" =>
                    fun _ ->
                        match getTest "testB" with
                        | Some name -> Assert.AreEqual("test B", name)
                        | _ -> Assert.Fail "no test found"
                yield "from type" =>
                    fun _ ->
                        match testFromType Dummy.thisModuleType.Value with
                        | Some (TestList t) ->
                            match Seq.toList t with
                            | [
                                TestLabel("test A", TestList _)
                                TestLabel("test B", TestList _)
                              ] -> ()
                            | x -> Assert.Fail (sprintf "TestList expected, found %A" x)
                        | x -> Assert.Fail (sprintf "TestList expected, found %A" x)
                yield "from empty type" =>
                    fun _ ->
                        let test = testFromType EmptyModule.thisModuleType.Value
                        Assert.AreEqual(None, test)
            ]

            "parse args" =>> [
                "default" => 
                    fun _ ->
                        let opts = parseArgs [||]
                        Assert.False opts.Parallel

                "parallel" => 
                    fun _ ->
                        let opts = parseArgs [|"/m"|]
                        Assert.True opts.Parallel
            ]
        ]

