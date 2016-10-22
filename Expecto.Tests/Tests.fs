module Expecto.Tests
#nowarn "46"

open System
open System.Threading
open System.IO
open Expecto
open Expecto.Impl
open FSharpx
open System.Globalization

module Dummy =
  open Expecto

  [<Tests>]
  let testA = TestLabel ("test A", TestList [])

  [<Tests>]
  let testB() = TestLabel ("test B", TestList [])

  let thisModuleType = lazy Type.GetType "Expecto.Tests+Dummy, Expecto.Tests"

module EmptyModule =
  let thisModuleType = lazy Type.GetType "Expecto.Tests+EmptyModule, Expecto.Tests"

let (==?) actual expected = Expect.equal expected actual ""

[<Tests>]
let tests =
  testList "all" [
    testCase "basic" <| fun _ ->
      Expect.equal 4 (2+2) "2+2"

    test "using computation expression" {
      Expect.equal 4 (2+2) "2+2"
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
      testCase "Expecto ignore" <| fun _ ->
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
  ]

[<Tests>]
let timeouts =
  testList "timeout" [
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
          Expect.isNone test ""
    ]

    testList "parse args" [
        testCase "default" <|
            fun _ ->
                let opts = parseArgs [||]
                opts.parallel ==? false

        testCase "parallel" <|
            fun _ ->
                let opts = parseArgs [|"/m"|]
                opts.parallel ==? true
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

        Expect.equal 3 results.Count "results count"

        Expect.isTrue (TestResult.isFailed results.["parse/en-US"]) "parse en-US fails"
        Expect.isTrue (TestResult.isPassed results.["parse/es-AR"]) "parse es-AR passes"
        Expect.isTrue (TestResult.isPassed results.["parse/fr-FR"]) "parse fr-FR passes"
    ]

    testList "expectations" [
      testList "notEqual" [
        testCase "pass" <| fun _ ->
          Expect.notEqual "" "monkey" "should be different"

        testCase "fail" <| fun _ ->
          let test () = Expect.notEqual "" "" "should fail"
          assertTestFails test
      ]

      testList "raise" [
        testCase "pass" <| fun _ ->
          Expect.throwsT<ArgumentNullException> (fun _ -> nullArg "")

        testCase "fail with incorrect exception" <| fun _ ->
          let test () = Expect.throwsT<ArgumentException> (fun _ -> nullArg "")
          assertTestFails test

        testCase "fail with no exception" <| fun _ ->
          let test () = Expect.throwsT<ArgumentNullException> ignore
          assertTestFails test
      ]

      testList "string contain" [
        testCase "pass" <| fun _ ->
          Expect.stringContains "hello world" "hello" "String actually contains"

        testCase "fail" <| fun _ ->
          let test () = Expect.stringContains "hello world" "a" "Deliberately failing"
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
        test "sample computation expr" {
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
          Expect.equal normalTag compexpTag "result"
    ]
  ]
