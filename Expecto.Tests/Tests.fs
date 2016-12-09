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
  let testA = TestLabel ("test A", TestList ([], Normal), Normal)

  [<Tests>]
  let testB() = TestLabel ("test B", TestList ([], Normal), Normal)

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

    testList "string comparison" [
      test "string equal" {
        Expect.equal "Test string" "Test string" "Test string"
      }

      test "fail - different length" {
        let format = "Failing - string with different length"
        let test () = Expect.equal "Test" "Test2" format
        let msg = sprintf "%s. String actual shorter than expected, at pos %i for expected '%A'." format 4 '2'
        assertTestFailsWithMsg msg (test, Normal)
      }

      test "fail - different content" {
        let format = "Failing - string with different content"
        let msg = sprintf "%s. String do not match at position %i. Expected: '%A', but got '%A'." format 3 't' '2'
        let test () = Expect.equal "Tes2" "Test" format
        assertTestFailsWithMsg msg (test, Normal)
      }
    ]

    testList "sumTestResults" [
      let sumTestResultsTests =
        [ { TestRunResult.name = ""; result = Passed; duration = TimeSpan.FromMinutes 2. }
          { TestRunResult.name = ""; result = TestResult.Error (ArgumentException()); duration = TimeSpan.FromMinutes 3. }
          { TestRunResult.name = ""; result = Failed ""; duration = TimeSpan.FromMinutes 4. }
          { TestRunResult.name = ""; result = Passed; duration = TimeSpan.FromMinutes 5. }
          { TestRunResult.name = ""; result = Failed ""; duration = TimeSpan.FromMinutes 6. }
          { TestRunResult.name = ""; result = Passed; duration = TimeSpan.FromMinutes 7. }
        ]
      let r = lazy sumTestResults sumTestResultsTests
      yield testCase "passed" <| fun _ ->
          r.Value.passed ==? 3
      yield testCase "failed" <| fun _ ->
          r.Value.failed ==? 2
      yield testCase "exn" <| fun _ ->
          r.Value.errored ==? 1
      yield testCase "duration" <| fun _ ->
          r.Value.duration ==? TimeSpan.FromMinutes 27.
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
          fun a b r -> r.passed = a.passed + b.passed
        yield testResultCountsSum "Ignored" <|
          fun a b r -> r.ignored = a.ignored + b.ignored
        yield testResultCountsSum "Failed" <|
          fun a b r -> r.failed = a.failed + b.failed
        yield testResultCountsSum "Errored" <|
          fun a b r -> r.errored = a.errored + b.errored
        yield testResultCountsSum "Time" <|
          fun a b r -> r.duration = a.duration + b.duration
      ]
      testCase "ToString" <| fun _ ->
        let c1 = { passed = 1; ignored = 5; failed = 2; errored = 3; duration = TimeSpan.FromSeconds 20. }
        c1.ToString() ==? "6 tests run: 1 passed, 5 ignored, 2 failed, 3 errored (00:00:20)\n"
    ]

    testList "Exception handling" [
      testCase "Expecto ignore" <| fun _ ->
        let test () = skiptest "b"
        let test = TestCase (test, Normal)
        match evalSilent test with
        | [{ result = Ignored "b" }] -> ()
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
        TestList ([
                    testCase "a" ignore
                    testCase "b" ignore
                    testList "c" [
                      testCase "d" ignore
                      testCase "e" ignore
                    ]
                  ], Normal)
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
        let test = TestCase(Test.timeout 10 (fun _ -> Thread.Sleep 100), Normal)
        let result = evalSilent test |> sumTestResults
        result.failed ==? 1
      testCase "pass" <| fun _ ->
        let test = TestCase(Test.timeout 1000 ignore, Normal)
        let result = evalSilent test |> sumTestResults
        result.passed ==? 1
    ]

    testList "Reflection" [
      let getMember name =
          Dummy.thisModuleType.Value.GetMember name
          |> Array.tryFind (fun _ -> true)
      let getTest =
          getMember
          >> Option.bind testFromMember
          >> Option.bind (function TestLabel(name, _, Normal) -> Some name | _ -> None)

      yield testCase "from member" <| fun _ ->
          getTest "testA" ==? Some "test A"
      yield testCase"from function" <| fun _ ->
          getTest "testB" ==? Some "test B"
      yield testCase"from type" <| fun _ ->
          match testFromType Dummy.thisModuleType.Value with
          | Some (TestList (
                      Seq.Two (
                          TestLabel("test B", TestList (_, Normal), Normal),
                          TestLabel("test A", TestList (_, Normal), Normal)), Normal)) -> ()
          | x -> failtestf "TestList expected, found %A" x
      yield testCase "from empty type" <| fun _ ->
          let test = testFromType EmptyModule.thisModuleType.Value
          Expect.isNone test ""
    ]

    testList "parse args" [
      testCase "default" <| fun _ ->
        let opts, isList = ExpectoConfig.fillFromArgs defaultConfig [||]
        opts.parallel ==? true
        isList ==? false

      testCase "sequenced" <| fun _ ->
        let opts, isList = ExpectoConfig.fillFromArgs defaultConfig [|"--sequenced"|]
        opts.parallel ==? false
        isList ==? false

      testCase "list" <| fun _ ->
        let _, isList = ExpectoConfig.fillFromArgs defaultConfig [|"--list-tests"|]
        isList ==? true
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
          |> Seq.map (fun r -> r.name, r.result)
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
          assertTestFails (test, Normal)
      ]

      testList "raise" [
        testCase "pass" <| fun _ ->
          Expect.throwsT<ArgumentNullException> (fun _ -> nullArg "")
                                                "Should throw null arg"

        testCase "fail with incorrect exception" <| fun _ ->
          let test () =
            Expect.throwsT<ArgumentException> (fun _ -> nullArg "")
                                              "Expected argument exception."

          assertTestFails (test, Normal)

        testCase "fail with no exception" <| fun _ ->
          let test () =
            Expect.throwsT<ArgumentNullException> ignore "Ignore 'should' throw an exn, ;)"
          assertTestFails (test, Normal)
      ]

      testList "string contain" [
        testCase "pass" <| fun _ ->
          Expect.stringContains "hello world" "hello" "String actually contains"

        testCase "fail" <| fun _ ->
          let test () = Expect.stringContains "hello world" "a" "Deliberately failing"
          assertTestFails (test, Normal)
      ]
    ]

    testList "computation expression" [
      let testNormal a =
        testCase "failing inside testCase" <| fun _ ->
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
          let normalTag = TestResult.tag normal.[0].result
          let compexpTag = TestResult.tag compexp.[0].result
          Expect.equal normalTag compexpTag "result"
    ]
  ]
