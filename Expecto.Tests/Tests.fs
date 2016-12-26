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

      test "different length, actual is shorter" {
        let format = "Failing - string with different length"
        let actual = "Test"
        let expected = "Test2"
        let diffString = "     ↑"
        let test () = Expect.equal actual expected format
        let msg =
          sprintf "%s.
          Expected string to equal:
          %A
          %s
          The string differs at index %d.
          %A
          %s
          String `actual` was shorter than expected, at pos %i for expected item %A."
            format expected diffString 4 actual diffString 4 '2'
        assertTestFailsWithMsg msg (test, Normal)
      }

      test "different length, actual is longer" {
        let format = "Failing - string with different length"
        let test () = Expect.equal "Test2" "Test" format
        let msg =
          sprintf """%s.
          Expected string to equal:
          "Test"
               ↑
          The string differs at index 4.
          "Test2"
               ↑
          String `actual` was longer than expected, at pos 4 found item '2'."""
            format
        assertTestFailsWithMsg msg (test, Normal)
      }

      test "fail - different content" {
        let format = "Failing - string with different content"
        let test () = Expect.equal "Test" "Tes2" format
        let diffString = "   ↑"
        let msg =
          sprintf """%s.
          Expected string to equal:
          "Tes2"
              ↑
          The string differs at index 3.
          "Test"
              ↑
          String does not match at position 3. Expected char: '2', but got 't'."""
            format
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
          r.Value.passed.Length ==? 3
      yield testCase "failed" <| fun _ ->
          r.Value.failed.Length ==? 2
      yield testCase "exn" <| fun _ ->
          r.Value.errored.Length ==? 1
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
          fun a b r -> r.passed = a.passed @ b.passed
        yield testResultCountsSum "Ignored" <|
          fun a b r -> r.ignored = a.ignored @ b.ignored
        yield testResultCountsSum "Failed" <|
          fun a b r -> r.failed = a.failed @ b.failed
        yield testResultCountsSum "Errored" <|
          fun a b r -> r.errored = a.errored @ b.errored
        yield testResultCountsSum "Time" <|
          fun a b r -> r.duration = a.duration + b.duration
      ]
      testCase "ToString" <| fun _ ->
        let c1 = { passed = [""]; ignored = [""; ""; ""; ""; ""]; failed = [""; ""]; errored = [""; ""; ""]; duration = TimeSpan.FromSeconds 20. }
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
let expecto =
  testList "expecto" [
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
        TestList (
          [
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
        result.failed.Length ==? 1
      testCase "pass" <| fun _ ->
        let test = TestCase(Test.timeout 1000 ignore, Normal)
        let result = evalSilent test |> sumTestResults
        result.passed.Length ==? 1
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

      testList "filtering" [
        let dummy =
          TestList (
            [
              testCase "a" ignore
              testCase "a_x" ignore
              testCase "b" ignore
              testList "c" [
                testCase "d" ignore
                testCase "e" ignore
                testList "f" [
                  testCase "g" ignore
                  testCase "h" ignore
                ]
              ]
            ], Normal)


        yield testCase "filter" <| fun _ ->
          let opts, _ =  ExpectoConfig.fillFromArgs defaultConfig [|"--filter"; "c"|]
          let filtered = dummy |> opts.filter |> Test.toTestCodeList
          filtered |> Seq.length ==? 4

        yield testCase "filter deep" <| fun _ ->
          let opts, _ =  ExpectoConfig.fillFromArgs defaultConfig [|"--filter"; "c/f"|]
          let filtered = dummy |> opts.filter |> Test.toTestCodeList
          filtered |> Seq.length ==? 2

        yield testCase "filter wrong" <| fun _ ->
          let opts, _ =  ExpectoConfig.fillFromArgs defaultConfig [|"--filter"; "f"|]
          let filtered = dummy |> opts.filter |> Test.toTestCodeList
          filtered |> Seq.length ==? 0

        yield testCase "filter test list" <| fun _ ->
          let opts, _ =  ExpectoConfig.fillFromArgs defaultConfig [|"--filter-test-list"; "f"|]
          let filtered = dummy |> opts.filter |> Test.toTestCodeList
          filtered |> Seq.length ==? 2

        yield testCase "filter test list wrong" <| fun _ ->
          let opts, _ =  ExpectoConfig.fillFromArgs defaultConfig [|"--filter-test-list"; "x"|]
          let filtered = dummy |> opts.filter |> Test.toTestCodeList
          filtered |> Seq.length ==? 0

        yield testCase "filter test case" <| fun _ ->
          let opts, _ =  ExpectoConfig.fillFromArgs defaultConfig [|"--filter-test-case"; "a"|]
          let filtered = dummy |> opts.filter |> Test.toTestCodeList
          filtered |> Seq.length ==? 2

        yield testCase "filter test case wrong" <| fun _ ->
          let opts, _ =  ExpectoConfig.fillFromArgs defaultConfig [|"--filter-test-case"; "y"|]
          let filtered = dummy |> opts.filter |> Test.toTestCodeList
          filtered |> Seq.length ==? 0

        yield testCase "run" <| fun _ ->
          let opts, _ =  ExpectoConfig.fillFromArgs defaultConfig [|"--run"; "a"; "c/d"; "c/f/h"|]
          let filtered = dummy |> opts.filter |> Test.toTestCodeList
          filtered |> Seq.length ==? 3


      ]

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

      testList "string starts" [
        testCase "pass" <| fun _ ->
          Expect.stringStarts "hello world" "hello" "String actually starts"

        testCase "fail" <| fun _ ->
          let test () = Expect.stringStarts "hello world" "a" "Deliberately failing"
          assertTestFails (test, Normal)
      ]

      testList "string ends" [
        testCase "pass" <| fun _ ->
          Expect.stringEnds "hello world" "world" "String actually ends"

        testCase "fail" <| fun _ ->
          let test () = Expect.stringEnds "hello world" "a" "Deliberately failing"
          assertTestFails (test, Normal)
      ]

      testList "string has length" [
        testCase "pass" <| fun _ ->
          Expect.stringHasLength "hello" 5 "String actually has length"

        testCase "fail" <| fun _ ->
          let test () = Expect.stringHasLength "hello world" 5 "Deliberately failing"
          assertTestFails (test, Normal)
      ]

      testList "#containsAll" [
        testCase "identical sequence" <| fun _ ->
          Expect.containsAll [|21;37|] [|21;37|] "Identical"

        testCase "sequence contains all in different order" <| fun _ ->
          Expect.containsAll [|21;37|] [|37;21|]
                             "Same elements in different order"

        testCase "sequence contains everything expected" <| fun _ ->
          let format = "Sequence should contain one and five"
          let test () =
            Expect.containsAll [|2; 1; 3|] [| 1; 5 |] format
          let msg =
            sprintf "%s.
    Sequence `actual` does not contain all `expected` elements.
        All elements in `actual`:
        {1, 2, 3}
        All elements in `expected`:
        {1, 5}
        Missing elements from `actual`:
        {5}
        Extra elements in `actual`:
        {2, 3}"
              format
          assertTestFailsWithMsg msg (test, Normal)
      ]

      testList "#distributed" [
        testCase "identical sequence" <| fun _ ->
          Expect.distributed [|21;37|] [|21;37|] "Identical"

        testCase "sequence contains all in different order" <| fun _ ->
          Expect.distributed [|21;37|] [|37;21|]
                             "Same elements in different order"

        testCase "sequence doesn't contain repeats in expected" <| fun _ ->
          let format = "Sequence should contain one, two and four"
          let test () =
            Expect.distributed [|2; 2|] [| 2; 1; 4 |] format
          let msg =
            sprintf "%s.
    Sequence `actual` does not contain all `expected` elements.
        All elements in `actual`:
        {2, 2}
        All elements in `expected`:
        {1, 2, 4}
        Missing elements from `actual`:
        {1, 4}
        Extra elements in `actual`:
        {2}"
              format
          assertTestFailsWithMsg msg (test, Normal)

        testCase "sequence does contain repeats in expected but should not" <| fun _ ->
          let format = "Sequence should contain two, two and four"
          let test () =
            Expect.distributed [|2; 2|] [| 2; 2; 4 |] format
          let msg =
            sprintf "%s.
    Sequence `actual` does not contain all `expected` elements.
        All elements in `actual`:
        {2, 2}
        All elements in `expected`:
        {2, 2, 4}
        Missing elements from `actual`:
        {4}
        Extra elements in `actual`:
        {}"
              format
          assertTestFailsWithMsg msg (test, Normal)

        testCase "sequence contains everything expected" <| fun _ ->
          let format = "Sequence should contain two and two"
          let test () =
            Expect.distributed [|2; 2; 4|] [| 2; 4 |] format
          let msg =
            sprintf "%s.
    Sequence `actual` does not contain all `expected` elements.
        All elements in `actual`:
        {2, 2, 4}
        All elements in `expected`:
        {2, 4}
        Missing elements from `actual`:
        {}
        Extra elements in `actual`:
        {2}"
              format
          assertTestFailsWithMsg msg (test, Normal)
      ]

      testList "sequence equal" [
        testCase "pass" <| fun _ ->
          Expect.sequenceEqual [1;2;3] [1;2;3] "Sequences actually equal"

        testCase "fail - longer" <| fun _ ->
          let test () = Expect.sequenceEqual [1;2;3] [1] "Deliberately failing"
          assertTestFails (test, Normal)

        testCase "fail - shorter" <| fun _ ->
          let test () = Expect.sequenceEqual [1] [1;2;3] "Deliberately failing"
          assertTestFails (test, Normal)
      ]

      testList "sequence starts" [
        testCase "pass" <| fun _ ->
          Expect.sequenceStarts [1;2;3] [1;2] "Sequences actually starts"

        testCase "fail - different" <| fun _ ->
          let test () = Expect.sequenceStarts [1;2;3] [2] "Deliberately failing"
          assertTestFails (test, Normal)

        testCase "fail - subject shorter" <| fun _ ->
          let test () = Expect.sequenceStarts [1] [1;2;3] "Deliberately failing"
          assertTestFails (test, Normal)
      ]

      testList "sequence ascending" [
        testCase "pass" <| fun _ ->
          Expect.isAscending [1;2;3] "Sequences actually ascending"

        testCase "fail " <| fun _ ->
          let test () = Expect.isAscending [1;3;2] "Deliberately failing"
          assertTestFails (test, Normal)
      ]

      testList "sequence descending" [
        testCase "pass" <| fun _ ->
          Expect.isDescending [3;2;1] "Sequences actually descending"

        testCase "fail " <| fun _ ->
          let test () = Expect.isDescending [3;1;2] "Deliberately failing"
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