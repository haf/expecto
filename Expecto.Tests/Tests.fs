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
        Expect.equal "Test" "Test2" "Failing - string with different length"
      } |> assertTestFailsWithMsgStarting "Failing - string with different length.\n          Expected string to equal:\n          \"Test2\"\n               ↑\n          The string differs at index 4.\n          \"Test\"\n               ↑\n          String `actual` was shorter than expected, at pos 4 for expected item '2'."

      test "different length, actual is longer" {
        Expect.equal "Test2" "Test" "Failing - string with different length"
      } |> assertTestFailsWithMsgStarting "Failing - string with different length.\n          Expected string to equal:\n          \"Test\"\n               ↑\n          The string differs at index 4.\n          \"Test2\"\n               ↑\n          String `actual` was longer than expected, at pos 4 found item '2'."

      test "fail - different content" {
        Expect.equal "Test" "Tes2" "Failing - string with different content"
      } |> assertTestFailsWithMsgStarting "Failing - string with different content.\n          Expected string to equal:\n          \"Tes2\"\n              ↑\n          The string differs at index 3.\n          \"Test\"\n              ↑\n          String does not match at position 3. Expected char: '2', but got 't'."
    ]

    testList "sumTestResults" [
      let sumTestResultsTests =
        [ { TestRunResult.name = ""; result = Passed; duration = TimeSpan.FromMinutes 2.; location = SourceLocation.empty }
          { TestRunResult.name = ""; result = TestResult.Error (ArgumentException()); duration = TimeSpan.FromMinutes 3.; location =SourceLocation.empty }
          { TestRunResult.name = ""; result = Failed ""; duration = TimeSpan.FromMinutes 4.; location = SourceLocation.empty }
          { TestRunResult.name = ""; result = Passed; duration = TimeSpan.FromMinutes 5.; location = SourceLocation.empty }
          { TestRunResult.name = ""; result = Failed ""; duration = TimeSpan.FromMinutes 6.; location = SourceLocation.empty }
          { TestRunResult.name = ""; result = Passed; duration = TimeSpan.FromMinutes 7.; location = SourceLocation.empty }
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
        let tr = {
          name = ""
          location = SourceLocation.empty
          result = Passed
          duration = TimeSpan.MaxValue
        }
        let c1 = { passed = [tr]; ignored = [tr; tr; tr; tr; tr]; failed = [tr; tr]; errored = [tr; tr; tr]; duration = TimeSpan.FromSeconds 20. }
        c1.ToString() ==? "6 tests run: 1 passed, 5 ignored, 2 failed, 3 errored (00:00:20)\n"
    ]

    testList "Exception handling" [
      testCaseAsync "Expecto ignore" <| async {
        let test () = skiptest "b"
        let test = TestCase (Sync test, Normal)
        let! result = Impl.evalSilentAsync test
        match result with
        | [{ result = Ignored "b" }] -> ()
        | x -> failtestf "Expected result = Ignored, got\n %A" x
      }
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

    testList "testParam 1" (
      testParam 1333 [
        "First sample",
          fun value () ->
            Expect.equal value 1333 "Should be expected value"
        "Second sample",
          fun value () ->
            Expect.isLessThan value 1444 "Should be less than"
    ] |> List.ofSeq)

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

    testSequenced <| testList "Timeout" [
      testCaseAsync "fail" <| async {
        let test = TestCase(Async.Sleep 100 |> Async |> Test.timeout 10, Normal)
        let! result = Impl.evalSilentAsync test
        (sumTestResults result).failed.Length ==? 1
      }
      testCaseAsync "pass" <| async {
        let test = TestCase(Sync ignore |> Test.timeout 1000, Normal)
        let! result = Impl.evalSilentAsync test
        (sumTestResults result).passed.Length ==? 1
      }
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
        match ExpectoConfig.fillFromArgs defaultConfig [||] with
        | ArgsRun opts ->
          opts.parallel ==? true
        | _ -> 0 ==? 1

      testCase "sequenced" <| fun _ ->
        match ExpectoConfig.fillFromArgs defaultConfig [|"--sequenced"|] with
        | ArgsRun opts ->
          opts.parallel ==? false
        | _ -> 0 ==? 1

      testCase "list" <| fun _ ->
        match ExpectoConfig.fillFromArgs defaultConfig [|"--list-tests"|] with
        | ArgsList _ -> ()
        | _ -> 0 ==? 1

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

        let getArgsConfig = function | ArgsRun c -> c | _ -> failwith "not normal"

        yield testCase "filter" <| fun _ ->
          let opts =  ExpectoConfig.fillFromArgs defaultConfig [|"--filter"; "c"|] |> getArgsConfig
          let filtered = dummy |> opts.filter |> Test.toTestCodeList
          filtered |> Seq.length ==? 4

        yield testCase "filter deep" <| fun _ ->
          let opts =  ExpectoConfig.fillFromArgs defaultConfig [|"--filter"; "c/f"|] |> getArgsConfig
          let filtered = dummy |> opts.filter |> Test.toTestCodeList
          filtered |> Seq.length ==? 2

        yield testCase "filter wrong" <| fun _ ->
          let opts =  ExpectoConfig.fillFromArgs defaultConfig [|"--filter"; "f"|] |> getArgsConfig
          let filtered = dummy |> opts.filter |> Test.toTestCodeList
          filtered |> Seq.length ==? 0

        yield testCase "filter test list" <| fun _ ->
          let opts =  ExpectoConfig.fillFromArgs defaultConfig [|"--filter-test-list"; "f"|] |> getArgsConfig
          let filtered = dummy |> opts.filter |> Test.toTestCodeList
          filtered |> Seq.length ==? 2

        yield testCase "filter test list wrong" <| fun _ ->
          let opts =  ExpectoConfig.fillFromArgs defaultConfig [|"--filter-test-list"; "x"|] |> getArgsConfig
          let filtered = dummy |> opts.filter |> Test.toTestCodeList
          filtered |> Seq.length ==? 0

        yield testCase "filter test case" <| fun _ ->
          let opts =  ExpectoConfig.fillFromArgs defaultConfig [|"--filter-test-case"; "a"|] |> getArgsConfig
          let filtered = dummy |> opts.filter |> Test.toTestCodeList
          filtered |> Seq.length ==? 2

        yield testCase "filter test case wrong" <| fun _ ->
          let opts =  ExpectoConfig.fillFromArgs defaultConfig [|"--filter-test-case"; "y"|] |> getArgsConfig
          let filtered = dummy |> opts.filter |> Test.toTestCodeList
          filtered |> Seq.length ==? 0

        yield testCase "run" <| fun _ ->
          let opts =  ExpectoConfig.fillFromArgs defaultConfig [|"--run"; "a"; "c/d"; "c/f/h"|] |> getArgsConfig
          let filtered = dummy |> opts.filter |> Test.toTestCodeList
          filtered |> Seq.length ==? 3


      ]

    ]

    testList "transformations" [
      testCaseAsync "multiple cultures" <| async {
        let withCulture culture test =
          fun () ->
            let c = Thread.CurrentThread.CurrentCulture
            try
              Thread.CurrentThread.CurrentCulture <- culture
              match test with
              | Sync test -> test()
              | Async test -> Async.StartImmediate test
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

        let! results = Impl.evalSilentAsync culturizedTests

        let results =
          results
          |> Seq.map (fun r -> r.name, r.result)
          |> Map.ofSeq

        Expect.equal 3 results.Count "results count"

        Expect.isTrue (TestResult.isFailed results.["parse/en-US"]) "parse en-US fails"
        Expect.isTrue (TestResult.isPassed results.["parse/es-AR"]) "parse es-AR passes"
        Expect.isTrue (TestResult.isPassed results.["parse/fr-FR"]) "parse fr-FR passes"
      }
    ]

    testList "expectations" [
      testList "notEqual" [
        testCase "pass" <| fun _ ->
          Expect.notEqual "" "monkey" "should be different"

        testCase "fail" (fun _ ->
          Expect.notEqual "" "" "should fail"
        ) |> assertTestFails
      ]

      testList "raise" [

        testCase "pass" <| fun _ ->
          Expect.throwsT<ArgumentNullException> (fun _ -> nullArg "")
                                                "Should throw null arg"

        testCase "fail with incorrect exception" (fun _ ->
          Expect.throwsT<ArgumentException> (fun _ -> nullArg "")
                                            "Expected argument exception."
        ) |> assertTestFails

        testCase "fail with no exception" (fun _ ->
          Expect.throwsT<ArgumentNullException> ignore "Ignore 'should' throw an exn, ;)"
        ) |> assertTestFails

      ]

      testList "string contain" [
        testCase "pass" <| fun _ ->
          Expect.stringContains "hello world" "hello" "String actually contains"

        testCase "fail" (fun _ ->
          Expect.stringContains "hello world" "a" "Deliberately failing"
        ) |> assertTestFails
      ]

      testList "string starts" [
        testCase "pass" <| fun _ ->
          Expect.stringStarts "hello world" "hello" "String actually starts"

        testCase "fail" (fun _ ->
          Expect.stringStarts "hello world" "a" "Deliberately failing"
        ) |> assertTestFails
      ]

      testList "string ends" [
        testCase "pass" <| fun _ ->
          Expect.stringEnds "hello world" "world" "String actually ends"

        testCase "fail" (fun _ ->
          Expect.stringEnds "hello world" "a" "Deliberately failing"
        ) |> assertTestFails
      ]

      testList "string has length" [
        testCase "pass" <| fun _ ->
          Expect.stringHasLength "hello" 5 "String actually has length"

        testCase "fail" (fun _ ->
          Expect.stringHasLength "hello world" 5 "Deliberately failing"
        ) |> assertTestFails
      ]

      testList "#containsAll" [
        testCase "identical sequence" <| fun _ ->
          Expect.containsAll [|21;37|] [|21;37|] "Identical"

        testCase "sequence contains all in different order" <| fun _ ->
          Expect.containsAll [|21;37|] [|37;21|]
                             "Same elements in different order"

        testCase "sequence contains everything expected" (fun _ ->
          Expect.containsAll [|2; 1; 3|] [| 1; 5 |]
                      "Sequence should contain one and five"
        ) |> assertTestFailsWithMsgStarting "Sequence should contain one and five.\n    Sequence `actual` does not contain all `expected` elements.\n        All elements in `actual`:\n        {1, 2, 3}\n        All elements in `expected`:\n        {1, 5}\n        Missing elements from `actual`:\n        {5}\n        Extra elements in `actual`:\n        {2, 3}"
      ]

      testList "#distribution" [
        testCase "identical sequence" <| fun _ ->
          Expect.distribution [21;37] (Map [21,1ul; 37,1ul])
            "Identical"

        testCase "sequence contains all in different order" <| fun _ ->
          Expect.distribution [21;37] (Map [37,1ul; 21,1ul])
            "Same elements in different order"

        testCase "sequence doesn't contain repeats in expected" (fun _ ->
          Expect.distribution [2;2;4] (Map [2,1ul; 1,1ul; 4,2ul])
            "Sequence should contain one, two and four"
        ) |> assertTestFailsWithMsgStarting "Sequence should contain one, two and four.\n    Sequence `actual` does not contain every `expected` elements.\n        All elements in `actual`:\n        {2, 2, 4}\n        All elements in `expected` ['item', 'number of expected occurrences']:\n        {1: 1, 2: 1, 4: 2}\n\tMissing elements from `actual`:\n\t'1' (0/1)\n\t'4' (1/2)\n\tExtra elements in `actual`:\n\t'2' (2/1)"

        testCase "sequence does contain repeats in expected but should not" (fun _ ->
          Expect.distribution [2;2] (Map [2,2ul; 4,1ul])
            "Sequence should contain two, two and four"
        ) |> assertTestFailsWithMsgStarting "Sequence should contain two, two and four.\n    Sequence `actual` does not contain every `expected` elements.\n        All elements in `actual`:\n        {2, 2}\n        All elements in `expected` ['item', 'number of expected occurrences']:\n        {2: 2, 4: 1}\n\tMissing elements from `actual`:\n\t'4' (0/1)"

        testCase "sequence does not contains everything expected" (fun _ ->
          Expect.distribution [2;2;4] (Map [2,1ul; 4,1ul])
            "Sequence should contain two and two"
        ) |> assertTestFailsWithMsgStarting "Sequence should contain two and two.\n    Sequence `actual` does not contain every `expected` elements.\n        All elements in `actual`:\n        {2, 2, 4}\n        All elements in `expected` ['item', 'number of expected occurrences']:\n        {2: 1, 4: 1}\n\tExtra elements in `actual`:\n\t'2' (2/1)"
      ]

      testList "sequence equal" [
        testCase "pass" <| fun _ ->
          Expect.sequenceEqual [1;2;3] [1;2;3] "Sequences actually equal"

        testCase "fail - longer" (fun _ ->
          Expect.sequenceEqual [1;2;3] [1] "Deliberately failing"
        ) |> assertTestFails

        testCase "fail - shorter" (fun _ ->
          Expect.sequenceEqual [1] [1;2;3] "Deliberately failing"
        ) |> assertTestFails
      ]

      testList "sequence starts" [
        testCase "pass" <| fun _ ->
          Expect.sequenceStarts [1;2;3] [1;2] "Sequences actually starts"

        testCase "fail - different" (fun _ ->
          Expect.sequenceStarts [1;2;3] [2] "Deliberately failing"
        ) |> assertTestFails

        testCase "fail - subject shorter" (fun _ ->
          Expect.sequenceStarts [1] [1;2;3] "Deliberately failing"
        ) |> assertTestFails
      ]

      testList "sequence ascending" [
        testCase "pass" <| fun _ ->
          Expect.isAscending [1;2;3] "Sequences actually ascending"

        testCase "fail " (fun _ ->
          Expect.isAscending [1;3;2] "Deliberately failing"
        ) |> assertTestFails
      ]

      testList "sequence descending" [
        testCase "pass" <| fun _ ->
          Expect.isDescending [3;2;1] "Sequences actually descending"

        testCase "fail " (fun _ ->
          Expect.isDescending [3;1;2] "Deliberately failing"
        ) |> assertTestFails
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
        testCaseAsync (sprintf "compare comp.exp. and normal with value %d" c) <| async {
          let! normal = testNormal c |> Impl.evalSilentAsync
          let! compexp = testCompExp c |> Impl.evalSilentAsync
          let normalTag = TestResult.tag normal.[0].result
          let compexpTag = TestResult.tag compexp.[0].result
          Expect.equal normalTag compexpTag "result"
        }
    ]
  ]

let inline popCount (i:uint16) =
  let mutable v = uint32 i
  v <- v - ((v >>> 1) &&& 0x55555555u)
  v <- (v &&& 0x33333333u) + ((v >>> 2) &&& 0x33333333u)
  ((v + (v >>> 4) &&& 0xF0F0F0Fu) * 0x1010101u) >>> 24

let inline popCount16 i =
  let mutable v = i - ((i >>> 1) &&& 0x5555us)
  v <- (v &&& 0x3333us) + ((v >>> 2) &&& 0x3333us)
  ((v + (v >>> 4) &&& 0xF0Fus) * 0x101us) >>> 8

[<Tests>]
let popcountTest =
  testList "performance" [
    testProperty "popcount same"
      (fun i -> (popCount i |> int) = (popCount16 i |> int))
  ]

[<Tests>]
let asyncTests =
  testList "async" [

    testCaseAsync "simple" <| async {
      Expect.equal 1 1 "1=1"
    }

    testCaseAsync "let" <| async {
      let! n = async { return 1 }
      Expect.equal 1 n "1=n"
    }

    testCaseAsync "can fail" <| async {
      let! n = async { return 2 }
      Expect.equal 1 n "1=n"
    } |> assertTestFails

  ]

[<Tests>]
let performance =
  testSequenced <| testList "performance" [

    testCase "1 <> 2" (fun _ ->
      Expect.isFasterThan (fun () -> 1) (fun () -> 2) "1 equals 2 should fail"
    )
    |> assertTestFailsWithMsgContaining "same"

    testCase "half is faster" <| fun _ ->
      Expect.isFasterThan (fun () -> repeat10000 log 76.0)
                          (fun () -> repeat10000 log 76.0 |> ignore; repeat10000 log 76.0)
                          "half is faster"

    testCase "double is faster should fail" (fun _ ->
      Expect.isFasterThan (fun () -> repeat10000 log 76.0 |> ignore; repeat10000 log 76.0)
                          (fun () -> repeat10000 log 76.0)
                          "double is faster should fail"
      ) |> assertTestFailsWithMsgContaining "slower"

    ptestCase "same function is faster should fail" (fun _ ->
      Expect.isFasterThan (fun () -> repeat100000 log 76.0)
                          (fun () -> repeat100000 log 76.0)
                          "same function is faster should fail"
      ) |> assertTestFailsWithMsgContaining "equal"

    testCase "matrix" <| fun _ ->
      let n = 100
      let rand = Random 123
      let a = Array2D.init n n (fun _ _ -> rand.NextDouble())
      let b = Array2D.init n n (fun _ _ -> rand.NextDouble())
      let c = Array2D.zeroCreate n n

      let reset() =
        for i = 0 to n-1 do
            for j = 0 to n-1 do
              c.[i,j] <- 0.0

      let mulIJK() =
        for i = 0 to n-1 do
          for j = 0 to n-1 do
            for k = 0 to n-1 do
              c.[i,k] <- c.[i,k] + a.[i,j] * b.[j,k]

      let mulIKJ() =
        for i = 0 to n-1 do
          for k = 0 to n-1 do
            let mutable t = 0.0
            for j = 0 to n-1 do
              t <- t + a.[i,j] * b.[j,k]
            c.[i,k] <- t
      Expect.isFasterThanSub (fun measurer -> reset(); measurer mulIKJ ())
                             (fun measurer -> reset(); measurer mulIJK ())
                             "ikj faster than ijk"

    testCase "popcount" (fun _ ->
      Expect.isFasterThan (fun () -> repeat10000 (popCount16 >> int) 987us)
                          (fun () -> repeat10000 (popCount >> int) 987us)
                          "popcount 16 faster than 32 fails"
      ) |> assertTestFailsWithMsgContaining "slower"
  ]

[<Tests>]
let close =
  testList "floatClose" [

    testCase "zero" <| fun _ ->
      Expect.floatClose Accuracy.veryHigh 0.0 0.0 "zero"

    testCase "small" <| fun _ ->
      Expect.floatClose Accuracy.low 0.000001 0.0 "small"

    testCase "large" <| fun _ ->
      Expect.floatClose Accuracy.low 10004.0 10000.0 "large"

    testCase "user" <| fun _ ->
      Expect.floatClose {absolute=0.0; relative=1e-3}
        10004.0 10000.0 "user"

    testCase "can fail" (fun _ ->
      Expect.floatClose Accuracy.low 1004.0 1000.0 "can fail"
    ) |> assertTestFails

    testCase "nan fails" (fun _ ->
      Expect.floatClose Accuracy.low nan 1.0 "nan fails"
    ) |> assertTestFails

    testCase "inf fails" (fun _ ->
      Expect.floatClose Accuracy.low infinity 1.0 "inf fails"
    ) |> assertTestFails

  ]