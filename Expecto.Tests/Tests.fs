module Expecto.Tests
#nowarn "46"

open System
open System.Text.RegularExpressions
open System.Threading
open System.IO
open System.Reflection
open Expecto
open Expecto.Impl
open Expecto.Logging
open System.Globalization

module Dummy =

  [<Tests>]
  let testA = TestLabel ("test A", TestList ([], Normal), Normal)

  [<Tests>]
  let testB() = TestLabel ("test B", TestList ([], Normal), Normal)

  let thisAssemblyName = "Expecto.Tests"
  let thisModuleNameQualified = sprintf "Expecto.Tests+Dummy, %s" thisAssemblyName
  let thisModuleType = lazy Type.GetType(thisModuleNameQualified, throwOnError=true)

module EmptyModule =
  let thisModuleNameQualified = sprintf "Expecto.Tests+EmptyModule, %s" Dummy.thisAssemblyName
  let thisModuleType = lazy Type.GetType(thisModuleNameQualified, throwOnError=true)

let (==?) actual expected = Expect.equal actual expected ""

type smallRecord = { a: string }
type anotherSmallRecord = { a: string; b:string }

[<Tests>]
let tests =

  testList "all" [
    testCase "basic" <| fun _ ->
      Expect.equal 4 (2+2) "2+2"

    test "using computation expression" {
      Expect.equal 4 (2+2) "2+2"
    }

    testAsync "using async computation expression" {
      Expect.equal 4 (2+2) "2+2"
    }

    testAsync "using async computation expression with bind" {
      let! x = async { return 4 }
      Expect.equal x (2+2) "2+2"
    }

    testList "testName tests" [
      testCase "one test" <| fun _ ->
        Expect.equal testName "all.testName tests.one test" "one name"
      testCase "two test" <| fun _ ->
        Expect.equal testName "all.testName tests.two test" "two name"
    ]

    testList "null comparison" [
      testCase "actual" (fun _ ->
        Expect.equal null (obj()) ""
      ) |> assertTestFails

      testCase "expected" (fun _ ->
        Expect.equal (obj()) null ""
      ) |> assertTestFails
    ]

    testList "null value tests" [
      testCase "null value passes isNullValue" (fun _ ->
        Expect.isNullValue (Nullable()) ""
      )

      testCase "null value fails isNotNullValue" (fun _ ->
        Expect.isNotNullValue (Nullable()) ""
      ) |> assertTestFails

      testCase "non-null value fails isNullValue" (fun _ ->
        Expect.isNullValue (Nullable 1) ""
      ) |> assertTestFails

      testCase "non-null value passes isNotNullValue" (fun _ ->
        Expect.isNotNullValue (Nullable 1) ""
      )
    ]

    testList "string comparison" [
      test "string equal" {
        Expect.equal "Test string" "Test string" "Test string"
      }

      test "different length, actual is shorter" {
        Expect.equal "Test" "Test2" "Failing - string with different length"
      } |> assertTestFailsWithMsgStartingDelay (fun () ->
            let redStart = ANSIOutputWriter.colourText (ANSIOutputWriter.getColour()) ConsoleColor.Red
            let redEnd = ANSIOutputWriter.colourText (ANSIOutputWriter.getColour()) ConsoleColor.Cyan
            let greenStart = ANSIOutputWriter.colourText (ANSIOutputWriter.getColour()) ConsoleColor.Green
            let greenEnd = ANSIOutputWriter.colourText (ANSIOutputWriter.getColour()) ConsoleColor.Cyan
            "Failing - string with different length. String actual was shorter than expected, at pos 4 for expected item '2'.\n"+
            greenStart+"expected"+greenEnd+": Test"+greenStart+"2"+greenEnd+"\n"+
            redStart+"  actual"+redEnd+": Test"
          )

      test "different length, actual is longer" {
        Expect.equal "Test2" "Test" "Failing - string with different length"
      } |> assertTestFailsWithMsgStartingDelay (fun () ->
            let redStart = ANSIOutputWriter.colourText (ANSIOutputWriter.getColour()) ConsoleColor.Red
            let redEnd = ANSIOutputWriter.colourText (ANSIOutputWriter.getColour()) ConsoleColor.Cyan
            let greenStart = ANSIOutputWriter.colourText (ANSIOutputWriter.getColour()) ConsoleColor.Green
            let greenEnd = ANSIOutputWriter.colourText (ANSIOutputWriter.getColour()) ConsoleColor.Cyan
            "Failing - string with different length. String actual was longer than expected, at pos 4 found item '2'.\n"+
            greenStart+"expected"+greenEnd+": Test\n"+
            redStart+"  actual"+redEnd+": Test"+redStart+"2"+redEnd
          )
      test "fail - different content" {
        Expect.equal "Test" "Tes2" "Failing - string with different content"
      } |> assertTestFailsWithMsgStartingDelay (fun () ->
            let redStart = ANSIOutputWriter.colourText (ANSIOutputWriter.getColour()) ConsoleColor.Red
            let redEnd = ANSIOutputWriter.colourText (ANSIOutputWriter.getColour()) ConsoleColor.Cyan
            let greenStart = ANSIOutputWriter.colourText (ANSIOutputWriter.getColour()) ConsoleColor.Green
            let greenEnd = ANSIOutputWriter.colourText (ANSIOutputWriter.getColour()) ConsoleColor.Cyan
            "Failing - string with different content. String does not match at position 3. Expected char: '2', but got 't'.\n"+
            greenStart+"expected"+greenEnd+": Tes"+greenStart+"2"+greenEnd + "\n"+
            redStart+"  actual"+redEnd+": Tes"+redStart+"t"+redEnd
          )
    ]

    testList "record comparison" [
      test "record equal" {
        Expect.equal { a = "dd" } { a = "dd" } "Test record"
      }

      test "fail - different content" {
        Expect.equal {a = "dd"; b = "de" } {a = "dd"; b = "dw" } "Failing - record with different content"
      } |> assertTestFailsWithMsgStartingDelay (fun () ->
            let redStart = ANSIOutputWriter.colourText (ANSIOutputWriter.getColour()) ConsoleColor.Red
            let redEnd = ANSIOutputWriter.colourText (ANSIOutputWriter.getColour()) ConsoleColor.Cyan
            let greenStart = ANSIOutputWriter.colourText (ANSIOutputWriter.getColour()) ConsoleColor.Green
            let greenEnd = ANSIOutputWriter.colourText (ANSIOutputWriter.getColour()) ConsoleColor.Cyan
            "Failing - record with different content.\nRecord does not match at position 2 for field named `b`. Expected field with value: \"dw\", but got \"de\".\n"+
            greenStart+"expected"+greenEnd+":\n{ a = \"dd\"\n  b = \"d"+greenStart+"w"+greenEnd+"\" }\n"+
            redStart+"  actual"+redEnd+":\n{ a = \"dd\"\n  b = \"d"+redStart+"e"+redEnd+"\" }"
          )
    ]

    testList "sumTestResults" [
      let sumTestResultsTests =
        let dummyTest = {
          name = [String.Empty]
          test = Sync ignore
          state = Normal
          focusOn = false
          sequenced = Synchronous }
        [ TestSummary.single Passed 2.
          TestSummary.single (TestResult.Error (ArgumentException())) 3.
          TestSummary.single (Failed "") 4.
          TestSummary.single Passed 5.
          TestSummary.single (Failed "") 6.
          TestSummary.single Passed 7.
        ] |> List.map (fun r -> dummyTest,r)
      let r = lazy TestRunSummary.fromResults sumTestResultsTests
      yield testCase "passed" <| fun _ ->
          List.length r.Value.passed ==? 3
      yield testCase "failed" <| fun _ ->
          List.length r.Value.failed ==? 2
      yield testCase "exn" <| fun _ ->
          List.length r.Value.errored ==? 1
      yield testCase "duration" <| fun _ ->
          r.Value.duration ==? TimeSpan.FromMilliseconds 27.
    ]

#if FSCHECK_TESTS
    testList "TestResultCounts" [
      let inline testProp fn =
        let config =
          {FsCheckConfig.defaultConfig
            with arbitrary=[Generator.arbs]}
        testPropertyWithConfig config fn
      yield testProp "total"
        (fun (a:TestRunSummary) ->
           List.length a.passed +
           List.length a.ignored +
           List.length a.failed +
           List.length a.errored = List.length a.results
        )
      yield testProp "plus"
        (fun (a:TestRunSummary) (b:TestRunSummary) ->
           let ab =
             { results = List.append a.results b.results
               duration = a.duration + b.duration
               maxMemory = if a.maxMemory > a.memoryLimit then a.maxMemory else b.maxMemory
               memoryLimit = if a.maxMemory > a.memoryLimit then a.memoryLimit else b.memoryLimit
               timedOut = List.append a.timedOut b.timedOut }
           let test fn  =
             List.length (fn a) + List.length (fn b) = List.length (fn ab)
           Expect.isTrue (test (fun a -> a.passed)) "Passed"
           Expect.isTrue (test (fun a -> a.ignored)) "Ignored"
           Expect.isTrue (test (fun a -> a.failed)) "Failed"
           Expect.isTrue (test (fun a -> a.errored)) "Errored"
           Expect.equal (a.successful && b.successful) ab.successful "Successful"
           Expect.equal (a.errorCode ||| b.errorCode) ab.errorCode "ErrorCode"
           true
        )
    ]
#endif

    testList "Exception handling" [
      testCaseAsync "Expecto ignore" <| async {
        let test () = skiptest "b"
        let test = TestCase (Sync test, Normal)
        let! result = Impl.evalTestsSilent test
        match result with
        | [(_,{ result = Ignored "b" })] -> ()
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

  let testArgs i o = Expect.equal (Args.parseOptions options i) o "testArgs"

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

    testList "Setup & teardown 4" [
      let withMemoryStreamAsync f = async {
        use ms = new MemoryStream()
        do! f ms
      }

      yield! testFixtureAsync withMemoryStreamAsync [
        "can read",
          fun ms -> async { return ms.CanRead ==? true }
        "can write",
          fun ms -> async { return ms.CanWrite ==? true }
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

    testList "shuffle" [
      testAsync "array sort same" {
        let a = [|1;2;3;4;5;6;7|]
        let b = Array.copy a
        Array.shuffleInPlace b
        Array.sortInPlace b
        Expect.sequenceEqual a b "ordered"
      }
      testAsync "array different" {
          let a = [|1;2;3;4;5;6;7|]
          let b = Array.copy a
          let rec shuffle() =
            Array.shuffleInPlace a
            Array.shuffleInPlace b
            if Array.exists2 (=) a b then shuffle()
          shuffle()
          Array.sortInPlace a
          Array.sortInPlace b
          Expect.sequenceEqual a b "same sorted"
      }
      testAsync "test shuffle" {
        let test =
          testList "1" [
            testList "2" [
              testCase "4" ignore
              ptestCase "5" ignore
              ftestCase "6" ignore
            ]
            testList "3" [
              testCase "7" ignore |> testSequenced
              ptestCase "8" ignore |> testSequencedGroup "g"
            ]
          ]
        let tests =
          Test.shuffle (defaultConfig.joinWith.asString) test
          |> Test.toTestCodeList
          |> List.sortBy (fun i -> i.name)
          |> List.map (fun t -> t.name,t.state,t.focusOn,t.sequenced)
        Expect.sequenceEqual tests [
          ["1.2.4"],Normal,true,InParallel
          ["1.2.5"],Pending,true,InParallel
          ["1.2.6"],Focused,true,InParallel
          ["1.3.7"],Normal,true,Synchronous
          ["1.3.8"],Pending,true,SynchronousGroup "g"
        ] "flat tests"
      }
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
        let t = Test.filter (defaultConfig.joinWith.asString) ((=) ["a"]) tests |> Test.toTestCodeList |> Seq.toList
        t.Length ==? 1
      yield testCase "with nested testcase" <| fun _ ->
        let t = Test.filter (defaultConfig.joinWith.asString) (fun (s: string list) -> defaultConfig.joinWith.format s |> fun z -> z.Contains "d") tests |> Test.toTestCodeList |> Seq.toList
        t.Length ==? 1
      yield testCase "with one testlist" <| fun _ ->
        let t = Test.filter (defaultConfig.joinWith.asString) (fun (s: string list) -> defaultConfig.joinWith.format s |> fun z -> z.Contains "c") tests |> Test.toTestCodeList |> Seq.toList
        t.Length ==? 2
      yield testCase "with no results" <| fun _ ->
        let t = Test.filter (defaultConfig.joinWith.asString) ((=) ["z"]) tests |> Test.toTestCodeList |> Seq.toList
        t.Length ==? 0
    ]

    testSequenced <| testList "Timeout" [
      testCaseAsync "fail" <| async {
        let test = TestCase(Async.Sleep 100 |> Async |> Test.timeout 10, Normal)
        let! result = Impl.evalTestsSilent test
        let summary = { results = result
                        duration = TimeSpan.Zero
                        maxMemory = 0L
                        memoryLimit = 0L
                        timedOut = [] }
        Seq.length summary.failed ==? 1
      }
      testCaseAsync "pass" <| async {
        let test = TestCase(Sync ignore |> Test.timeout 1000, Normal)
        let! result = Impl.evalTestsSilent test
        let summary = { results = result
                        duration = TimeSpan.Zero
                        maxMemory = 0L
                        memoryLimit = 0L
                        timedOut = [] }
        Seq.length summary.passed ==? 1
      }
    ]

    testList "Reflection" [
      let getMember name =
          Dummy.thisModuleType.Value.GetTypeInfo().GetMember name
          |> Array.tryHead
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

    testList "args" [
      testAsync "empty" {
        testArgs [||] (Ok [])
      }
      testAsync "one" {
        testArgs [|"--sequenced"|] (Ok [Sequenced])
      }
      testAsync "two" {
        testArgs [|"--parallel";"--sequenced"|]
          (Ok [Parallel;Sequenced])
      }
      testAsync "three" {
        testArgs [|"--sequenced";"--stress";"1.2";"--parallel"|]
          (Ok [Sequenced;Stress 1.2;Parallel;])
      }
      testAsync "int" {
        testArgs [|"--parallel-workers";"3"|]
          (Ok [Parallel_Workers 3])
      }
      testAsync "int error" {
          testArgs [|"--fscheck-end-size";"1.2"|]
            (Result.Error ["--fscheck-end-size Cannot parse parameter '1.2'"])
      }
      testAsync "float" {
          testArgs [|"--stress-memory-limit";"3"|]
            (Ok [Stress_Memory_Limit 3.0])
      }
      testAsync "float error" {
          testArgs [|"--stress-memory-limit";"3sd"|]
            (Result.Error ["--stress-memory-limit Cannot parse parameter '3sd'"])
      }
      testAsync "missing string" {
        testArgs [|"--log-name";"--sequenced"|]
          (Result.Error ["--log-name requires a parameter"])
      }
      testAsync "missing int" {
          testArgs [|"--fscheck-max-tests";"--parallel"|]
            (Result.Error ["--fscheck-max-tests requires a parameter"])
        }
      testAsync "many" {
        testArgs [|"--run";"one";"two";"three"|]
          (Ok [Run ["one";"two";"three"]])
      }
      testAsync "many end" {
        testArgs [|"--run";"one";"two";"three";"--parallel"|]
          (Ok [Run ["one";"two";"three"];Parallel])
      }
      testAsync "unknown start" {
        testArgs [|"hello";"--parallel";"--sequenced"|]
          (Result.Error ["unknown options: hello"])
      }
      testAsync "unknown middle" {
        testArgs [|"--parallel";"hello";"--sequenced"|]
          (Result.Error ["unknown options: hello"])
      }
      testAsync "unknown end" {
        testArgs [|"--parallel";"--sequenced";"hello"|]
          (Result.Error ["unknown options: hello"])
      }
      testAsync "unknown all" {
        testArgs [|"one";"--parallel";"two";"--fscheck-max-tests";"42";"three"|]
          (Result.Error ["unknown options: one two three"])
      }
      testAsync "help" {
        testArgs [|"--help"|]
          (Result.Error [])
      }
      testAsync "help start" {
        testArgs [|"--help";"--sequenced"|]
          (Result.Error [])
      }
      testAsync "help end" {
        testArgs [|"--sequenced";"--help"|]
          (Result.Error [])
      }
      testAsync "help middle" {
        testArgs [|"--sequenced";"--help";"--parallel"|]
          (Result.Error [])
      }
      testAsync "help and unknown" {
        testArgs [|"one";"--sequenced";"two";"--help";"three";"--parallel";"four"|]
          (Result.Error ["unknown options: one two three four"])
      }
      testAsync "two errors" {
        testArgs [|"one";"--filter"|]
          (Result.Error ["--filter requires a parameter";"unknown options: one"])
      }
      testAsync "three errors" {
        testArgs [|"--fscheck-start-size";"bb";"one";"--filter"|]
          (Result.Error [
            "--fscheck-start-size Cannot parse parameter 'bb'"
            "--filter requires a parameter"
            "unknown options: one"
          ])
      }
      testAsync "lots" {
        let args = [|
            "--sequenced"
            "--parallel"
            "--parallel-workers"; "3"
            "--stress"; "0.1"
            "--stress-timeout"; "100.1"
            "--stress-memory-limit"; "128"
            "--fail-on-focused-tests"
            "--debug"
            "--log-name"; "fred"
            "--filter"; "phil"
            "--filter-test-list"; "f list"
            "--filter-test-case"; "f case"
            "--run"; "a"; "b"; "c"
            "--list-tests"
            "--list-tests"; "normal"; "FOCUSED"; "Pending"
            "--summary"
            "--version"
            "--summary-location"
            "--fscheck-max-tests"; "5"
            "--fscheck-start-size"; "10"
            "--fscheck-end-size"; "20"
            "--allow-duplicate-names"
            "--no-spinner"
            "--colours"; "256"
            "--join-with"; "."
        |]
        let ok = [
          Sequenced
          Parallel
          Parallel_Workers 3
          Stress 0.1
          Stress_Timeout 100.1
          Stress_Memory_Limit 128.0
          Fail_On_Focused_Tests
          CLIArguments.Debug
          Log_Name "fred"
          Filter "phil"
          Filter_Test_List "f list"
          Filter_Test_Case "f case"
          Run ["a";"b";"c"]
          List_Tests []
          List_Tests [Normal; Focused; Pending]
          Summary
          Version
          Summary_Location
          FsCheck_Max_Tests 5
          FsCheck_Start_Size 10
          FsCheck_End_Size 20
          Allow_Duplicate_Names
          No_Spinner
          Colours 256
          JoinWith "."
        ]
        testArgs args (Ok ok)
      }
    ]

    testList "parse args" [
      testCase "default" <| fun _ ->
        match ExpectoConfig.fillFromArgs defaultConfig [||] with
        | ArgsRun opts ->
          opts.runInParallel ==? true
        | _ -> 0 ==? 1

      testCase "sequenced" <| fun _ ->
        match ExpectoConfig.fillFromArgs defaultConfig [|"--sequenced"|] with
        | ArgsRun opts ->
          opts.runInParallel ==? false
        | _ -> 0 ==? 1

      testCase "list" <| fun _ ->
        match ExpectoConfig.fillFromArgs defaultConfig [|"--list-tests"|] with
        | ArgsList _ -> ()
        | _ -> 0 ==? 1

      testList "filtering" [
        let dummy fn =
          TestList (
            [
              testCase "a" fn
              testCase "a_x" fn
              testCase "b" fn
              testList "c" [
                testCase "d" fn
                testCase "e" fn
                testList "f" [
                  testCase "g" fn
                  testCase "h" fn
                ]
              ]
            ], Normal)

        let getArgsConfig = function | ArgsRun c -> c | _ -> failwith "not normal"

        yield testCase "filter" <| fun _ ->
          let opts =  ExpectoConfig.fillFromArgs defaultConfig [|"--filter"; "c"|] |> getArgsConfig
          let filtered = dummy ignore |> opts.filter |> Test.toTestCodeList
          filtered |> Seq.length ==? 4

        yield testCase "filter deep" <| fun _ ->
          let opts =  ExpectoConfig.fillFromArgs defaultConfig [|"--filter"; "c.f"|] |> getArgsConfig
          let filtered = dummy ignore |> opts.filter |> Test.toTestCodeList
          filtered |> Seq.length ==? 2

        yield testCase "filter wrong" <| fun _ ->
          let opts =  ExpectoConfig.fillFromArgs defaultConfig [|"--filter"; "f"|] |> getArgsConfig
          let filtered = dummy ignore |> opts.filter |> Test.toTestCodeList
          filtered |> Seq.length ==? 0

        yield testCase "filter test list" <| fun _ ->
          let opts =  ExpectoConfig.fillFromArgs defaultConfig [|"--filter-test-list"; "f"|] |> getArgsConfig
          let filtered = dummy ignore |> opts.filter |> Test.toTestCodeList
          filtered |> Seq.length ==? 2

        yield testCase "filter test list wrong" <| fun _ ->
          let opts =  ExpectoConfig.fillFromArgs defaultConfig [|"--filter-test-list"; "x"|] |> getArgsConfig
          let filtered = dummy ignore |> opts.filter |> Test.toTestCodeList
          filtered |> Seq.length ==? 0

        yield testCase "filter test case" <| fun _ ->
          let opts =  ExpectoConfig.fillFromArgs defaultConfig [|"--filter-test-case"; "a"|] |> getArgsConfig
          let filtered = dummy ignore |> opts.filter |> Test.toTestCodeList
          filtered |> Seq.length ==? 2

        yield testCase "filter test case wrong" <| fun _ ->
          let opts =  ExpectoConfig.fillFromArgs defaultConfig [|"--filter-test-case"; "y"|] |> getArgsConfig
          let filtered = dummy ignore |> opts.filter |> Test.toTestCodeList
          filtered |> Seq.length ==? 0

        yield testCase "run" <| fun _ ->
          let opts =  ExpectoConfig.fillFromArgs defaultConfig [|"--run"; "a"; "c.d"; "c.f.h"|] |> getArgsConfig
          let filtered = dummy ignore |> opts.filter |> Test.toTestCodeList
          filtered |> Seq.length ==? 3

        yield testCase "run with filter" <| fun _ ->
          let count = ref 0
          let test = dummy (fun () -> incr count)
          runTestsWithCLIArgs [No_Spinner] [|"--filter"; "c.f"|] test ==? 0

        yield testCase "run with filter test case" <| fun _ ->
          let count = ref 0
          let test = dummy (fun () -> incr count)
          runTestsWithCLIArgs [No_Spinner] [|"--filter-test-case"; "a"|] test ==? 0

        yield testCase "run with filter test list" <| fun _ ->
          let count = ref 0
          let test = dummy (fun () -> incr count)
          runTestsWithCLIArgs [No_Spinner] [|"--filter-test-list"; "f"|] test ==? 0

        yield testCase "run with join" <| fun _ ->
          let count = ref 0
          let test = dummy (fun () -> incr count)
          runTestsWithCLIArgs [No_Spinner] [|"--join-with"; "."|] test ==? 0

        yield testCase "run with run" <| fun _ ->
          let count = ref 0
          let test = dummy (fun () -> incr count)
          runTestsWithCLIArgs [No_Spinner] [|"--run"; "a"; "c.d"; "c.f.h"|] test ==? 0
      ]
    ]

    testList "transformations" [
      testCaseAsync "multiple cultures" <| async {
        let getCurrentCulture () : CultureInfo =
#if NETCOREAPP1_1 || NETCOREAPP2_0
          System.Globalization.CultureInfo.CurrentCulture
#else
          System.Threading.Thread.CurrentThread.CurrentCulture
#endif

        let setCurrentCulture (culture : CultureInfo) =
#if NETCOREAPP1_1 || NETCOREAPP2_0
          System.Globalization.CultureInfo.CurrentCulture <- culture
#else
          System.Threading.Thread.CurrentThread.CurrentCulture <- culture
#endif

        let withCulture culture test =
          async {
            let c = getCurrentCulture()
            try
              setCurrentCulture culture
              match test with
              | Sync test ->
                test()
              | SyncWithCancel test ->
                test CancellationToken.None
              | Async test ->
                do! test
              | AsyncFsCheck (config, _, test) ->
                let configOrDefault = match config with | Some c -> c | _ -> FsCheckConfig.defaultConfig
                do! configOrDefault |> test
            finally
              setCurrentCulture c
          }

        let testWithCultures (cultures: #seq<CultureInfo>) =
          Test.replaceTestCode <| fun name test ->
            testList (name) [
              for c in cultures ->
                testCaseAsync c.Name (withCulture c test)
            ]

        let atest = test "parse" {
          Single.Parse("123,33") ==? 123.33f
        }

        let cultures =
          ["en-US"; "es-AR"; "fr-FR"]
          |> List.map CultureInfo

        let culturizedTests = testWithCultures cultures atest

        let! results = Impl.evalTestsSilent culturizedTests

        let results =
          results
          |> Seq.map (fun (t,r) -> t.name, r.result)
          |> Map.ofSeq

        Expect.equal 3 results.Count "results count"

        Expect.isTrue (results.[["parse";"en-US"]].isFailed) "parse en-US fails"
        Expect.isTrue (results.[["parse";"es-AR"]].isPassed) "parse es-AR passes"
        Expect.isTrue (results.[["parse";"fr-FR"]].isPassed) "parse fr-FR passes"
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

      testList "throws" [

        testCase "pass" <| fun _ ->
          Expect.throws (fun _ -> nullArg "") "Expected to throw an exception"

        testCase "fail when exception is not raised" (fun _ ->
          Expect.throws ignore "Should fail because no exception is thrown"
        ) |> assertTestFails
      ]

      testList "throwsC" [

        testCase "pass and call 'cont' when exception is raised" <| fun _ ->
          let mutable contCalled = false
          let exc = Exception()
          Expect.throwsC
            (fun _ -> raise exc)
            (fun e ->
              contCalled <- true
              if e <> exc then failtest "passes different exception"
            )

          if not contCalled then failtest "'cont' is not called"

        testCase "fail when exception is not raised" (fun _ ->
          Expect.throwsC ignore ignore
        ) |> assertTestFails

        testCase "do not call 'cont' if exception is not raised" <| fun _ ->
          let mutable contCalled = false
          try
            Expect.throwsC ignore (fun e -> contCalled <- true)
          with
            _ -> ()

          if contCalled then failtest "should not call 'cont'"
      ]

      testList "throwsT" [

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

        testCase "give correct assert message on no exception" (fun _ ->
          Expect.throwsT<ArgumentNullException> ignore "Should throw null arg"
        ) |> assertTestFailsWithMsgContaining "Expected f to throw."

      ]

      testList "throwsAsync" [

        testCaseAsync "pass" <|
          Expect.throwsAsync (fun _ -> nullArg "") "Expected to throw an exception"

        testCaseAsync "fail when exception is not raised" (
          Expect.throwsAsync (fun _ -> async { () }) "Should fail because no exception is thrown"
        ) |> assertTestFails
      ]

      testList "throwsAsyncC" [

        testCaseAsync "pass and call 'cont' when exception is raised" <| async {
          let mutable contCalled = false
          let exc = Exception()
          do!
            Expect.throwsAsyncC
              (fun _ -> raise exc)
              (fun e -> async {
                contCalled <- true
                if e <> exc then failtest "passes different exception"
              })

          if not contCalled then failtest "'cont' is not called"
        }

        testCaseAsync "fail when exception is not raised" (
          Expect.throwsAsyncC (fun _ -> async { () }) (fun _ -> async { () })
        ) |> assertTestFails

        testCaseAsync "do not call 'cont' if exception is not raised" <| async {
          let mutable contCalled = false
          try
            do! Expect.throwsAsyncC (fun _ -> async { () }) (fun e -> async { contCalled <- true })
          with
            _ -> ()

          if contCalled then failtest "should not call 'cont'"
        }
      ]

      testList "throwsAsyncT" [

        testCaseAsync "pass" <|
          Expect.throwsAsyncT<ArgumentNullException> (fun _ -> nullArg "")
                                                     "Should throw null arg"

        testCaseAsync "fail with incorrect exception" (
          Expect.throwsAsyncT<ArgumentException> (fun _ -> async { nullArg "" })
                                                 "Expected argument exception."
        ) |> assertTestFails

        testCaseAsync "fail with no exception" (
          Expect.throwsAsyncT<ArgumentNullException> (fun _ -> async { () }) "Ignore 'should' throw an exn, ;)"
        ) |> assertTestFails

        testCaseAsync "give correct assert message on no exception" (
          Expect.throwsAsyncT<ArgumentNullException> (fun _ -> async { () }) "Should throw null arg"
        ) |> assertTestFailsWithMsgContaining "Expected f to throw."

      ]

      testList "flipped throwsT" [

        testCase "pass" <| fun _ ->
          (fun _ -> nullArg "") |> Flip.Expect.throwsT<ArgumentNullException>
                                                "Should throw null arg"

        testCase "fail with incorrect exception" (fun _ ->
          (fun _ -> nullArg "") |> Flip.Expect.throwsT<ArgumentException>
                                            "Expected argument exception."
        ) |> assertTestFails

        testCase "fail with no exception" (fun _ ->
          ignore |> Flip.Expect.throwsT<ArgumentNullException> "Ignore 'should' throw an exn, ;)"
        ) |> assertTestFails

      ]

      testList "flipped throwsAsyncT" [

        testCaseAsync "pass" <| (
          (fun _ -> nullArg "") |> Flip.Expect.throwsAsyncT<ArgumentNullException>
                                                "Should throw null arg"
        )

        testCaseAsync "fail with incorrect exception" (
          (fun _ -> nullArg "") |> Flip.Expect.throwsAsyncT<ArgumentException>
                                            "Expected argument exception."
        ) |> assertTestFails

        testCaseAsync "fail with no exception" (
          (fun _ -> async { () }) |> Flip.Expect.throwsAsyncT<ArgumentNullException> "Ignore 'should' throw an exn, ;)"
        ) |> assertTestFails

      ]

      testList "double" [
        testList "nan testing" [
          testCase "is not 'NaN'" <| fun _ ->
            Expect.isNotNaN 4.0 "should pass because it's not 'Nan'"
          testCase "is 'NaN'" (fun _ ->
            Expect.isNotNaN Double.NaN "should fail because it's 'NaN'"
           ) |> assertTestFails
        ]

        testList "positive infinity testing" [
          testCase "is not a positive infinity" <| fun _ ->
            Expect.isNotPositiveInfinity 4.0 "should pass because it's not positive infinity"
          testCase "is a positive infinity" (fun _ ->
            Expect.isNotPositiveInfinity Double.PositiveInfinity "should fail because it's a positive infinity"
           ) |> assertTestFails
        ]

        testList "negative infinity testing" [
          testCase "is not a negative infinity" <| fun _ ->
            Expect.isNotNegativeInfinity 4.0 "should pass because it's not a negative infinity"
          testCase "is a negative infinity" (fun _ ->
            Expect.isNotNegativeInfinity Double.NegativeInfinity "should fail because it's negative infinity"
           ) |> assertTestFails
        ]

        testList "infinity testing" [
          testCase "is not an infinity" <| fun _ ->
            Expect.isNotInfinity 4.0 "should pass because it's not an negative infinity nor positive"

          testCase "is a negative infinity" (fun _ ->
            Expect.isNotInfinity Double.NegativeInfinity "should fail because it's negative infinity"
           ) |> assertTestFails

          testCase "is a positive infinity" (fun _ ->
            Expect.isNotInfinity Double.PositiveInfinity "should fail because it's positive infinity"
           ) |> assertTestFails
        ]
      ]

      testList "string isnotempty" [
        testCase "when string is not empty" <| fun _ ->
          Expect.isNotEmpty "dede" "should pass because string is not empty"

        testCase "when string is empty" (fun _ ->
          Expect.isNotEmpty "" "should fail because string is empty"
        ) |> assertTestFails

        testCase "when string is whitespace" (fun _ ->
          Expect.isNotEmpty "             " "should pass because string is whitespace"
        )
      ]

      testList "string isnotwhitespace" [
        testCase "when string is not whitespace" <| fun _ ->
          Expect.isNotEmpty "  dede  " "should pass because string is not whitespace"

        testCase "when string is empty" (fun _ ->
          Expect.isNotEmpty "" "should fail because string is empty"
        ) |> assertTestFails
      ]

      testList "string matches pattern for isMatch" [
        testCase "pass" <| fun _ ->
          Expect.isMatch "{function:45}" "{function:(\\d+)}" "string matches passed pattern"

        testCase "fail" (fun _ ->
          Expect.isMatch "{function:45d}" "{function:(\\d+)}" "Deliberately failing"
        ) |> assertTestFails
      ]

      testList "string matches pattern for isNotMatch" [
        testCase "pass" <| fun _ ->
          Expect.isNotMatch "{function:45d}" "{function:(\\d+)}" "string not matches passed pattern"

        testCase "fail" (fun _ ->
          Expect.isNotMatch "{function:45}" "{function:(\\d+)}" "Deliberately failing"
        ) |> assertTestFails
      ]

      testList "string matches regex for isRegexMatch" [
        testCase "pass" <| fun _ ->
          let regex = Regex("{function:(\\d+)}")
          Expect.isRegexMatch "{function:45}" regex "string matches passed regex"

        testCase "fail" (fun _ ->
          let regex = Regex("{function:(\\d+)}")
          Expect.isRegexMatch "{function:45d}" regex "Deliberately failing"
        ) |> assertTestFails
      ]

      testList "string matches regex for isNotRegexMatch" [
        testCase "pass" <| fun _ ->
          let regex = Regex("{function:(\\d+)}")
          Expect.isNotRegexMatch "{function:45d}" regex "string not matches passed regex"

        testCase "fail" (fun _ ->
          let regex = Regex("{function:(\\d+)}")
          Expect.isNotRegexMatch "{function:45}" regex "Deliberately failing"
        ) |> assertTestFails
      ]

      testList "string matches pattern and groups collection operator" [
        testCase "pass" <| fun _ ->
          let assertionText = "niceone"
          let input = sprintf "{function:%s}" assertionText
          let operation (groups : GroupCollection) =
            groups.[1].Value = assertionText
          Expect.isMatchGroups input "{function:(.*)}" operation "string not matches passed regex"

        testCase "fail" (fun _ ->
          let operation (groups : GroupCollection) =
            groups.[1].Value = "uga buga"
          Expect.isMatchGroups "hehhehe" "{function:(.*)}" operation "Deliberately failing"
        ) |> assertTestFails
      ]

      testList "string matches regex and groups collection operator" [
        testCase "pass" <| fun _ ->
          let assertionText = "niceone"
          let input = sprintf "{function:%s}" assertionText
          let regex = Regex("{function:(.*)}")
          let operation (groups : GroupCollection) =
            groups.[1].Value = assertionText
          Expect.isMatchRegexGroups input regex operation "string not matches passed regex"

        testCase "fail" (fun _ ->
          let regex = Regex("{function:(\\d+)}")
          let operation (groups : GroupCollection) =
            groups.[1].Value = "uga buga"
          Expect.isMatchRegexGroups "hehhehe" regex operation "Deliberately failing"
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

      testList "seq has length" [
          testCase "pass" <| fun _ ->
            Expect.hasLength [1;2;3;4;5] 5 "Seq actually has length"

          testCase "fail" (fun _ ->
            Expect.hasLength [1;2;3;4;5] 4 "Deliberately failing"
          ) |> assertTestFails
        ]

      testList "list is empty" [
        testCase "pass" <| fun _ ->
          Expect.isEmpty [] "list is empty"

        testCase "fail" (fun _ ->
          Expect.isEmpty [5] "list is not empty"
        ) |> assertTestFails
      ]

      testList "array is empty" [
        testCase "pass" <| fun _ ->
          Expect.isEmpty [||] "list is empty"

        testCase "fail" (fun _ ->
          Expect.isEmpty [|5|] "list is not empty"
        ) |> assertTestFails
      ]

      testList "array is non empty" [
        testCase "pass" <| fun _ ->
          Expect.isNonEmpty [|5|] "list is non empty"

        testCase "fail" (fun _ ->
          Expect.isNonEmpty [||] "list is empty"
        ) |> assertTestFails
      ]

      testList "list is non empty" [
        testCase "pass" <| fun _ ->
          Expect.isNonEmpty [5] "list is non empty"

        testCase "fail" (fun _ ->
          Expect.isNonEmpty [] "list is empty"
        ) |> assertTestFails
      ]

      testList "list count" [
        testCase "pass" <| fun _ ->
          Expect.hasCountOf [2;2;4] 2u (fun x -> x = 2) "list has 2 occurrences of number 2"

        testCase "fail" (fun _ ->
          Expect.hasCountOf [2;3] 2u (fun x -> x = 2) "list has 1 occurrences of number 3"
        ) |> assertTestFails
      ]

      testList "#exists" [
        testCase "pass" <| fun _ ->
          Expect.exists [2;2;4] ((=) 2) "should pass"

        testCase "fail" (fun _ ->
          Expect.exists [2;3] ((=) 5) "should fail"
        ) |> assertTestFails

        testCase "null" (fun _ ->
          Expect.exists null ((=) 5) "should also fail"
        ) |> assertTestFails
      ]

      testList "#all" [
        testCase "pass" <| fun _ ->
          Expect.all [2;2] ((=) 2) "should pass"

        testCase "fail" (fun _ ->
          Expect.all [2;3] ((=) 2) "should fail"
        ) |> assertTestFails

        testCase "null" (fun _ ->
          Expect.all null ((=) 5) "should also fail"
        ) |> assertTestFails

        testCase "enumerating has side effects" <| fun _ ->
          let count = 10
          let mutable id = 0
          let tryCreate value =
            if id = count then failtest "sequence should be enumerated only once"
            id <- id + 1
            Some (id, value)
          Expect.all
            ([1..count] |> Seq.map tryCreate)
            Option.isSome
            "should pass"
      ]

      testList "#allEqual" [
        testCase "pass - int" <| fun _ ->
          Expect.allEqual [2;2] 2 "should pass"

        testCase "pass - string" <| fun _ ->
          Expect.allEqual ["dd";"dd"] "dd" "should pass"

        testCase "fail" (fun _ ->
          Expect.allEqual [2;3] 2 "should fail"
        ) |> assertTestFails

        testCase "null" (fun _ ->
          Expect.allEqual null 5 "should also fail"
        ) |> assertTestFails

        testCase "enumerating has side effects" <| fun _ ->
          let count = 10
          let mutable id = 0
          let tryCreate value =
            if id = count then failtest "sequence should be enumerated only once"
            id <- id + 1
            Some (id, value)
          Expect.allEqual
            ([1..count] |> Seq.map (tryCreate >> Option.isSome))
            true
            "should pass"
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

      testList "#sequenceContainsOrder" [
        testCase "Valid ordering of subsequence" <| fun _ ->
          Expect.sequenceContainsOrder [ 1; 2; 3; 4; 5; 6 ] [ 1; 3; 5 ] "should pass"

        testCase "Wrong order of 0th and 1th elem" (fun _ ->
          Expect.sequenceContainsOrder [ 1; 2; 3; 4; 5; 6 ] [ 3; 1; 6 ] "should fail"
        ) |> assertTestFails

        testCase "Missing 7 from actual" (fun _ ->
          Expect.sequenceContainsOrder [ 1; 2; 3; 4; 5; 6 ] [ 1; 3; 7 ] "should fail"
        ) |> assertTestFails

        testCase "Empty list passes" <| fun _ ->
          Expect.sequenceContainsOrder [ 1; 2; 3; 4; 5; 6 ] [ ] "should pass"
      ]

      testList "sequence equal" [
        testCase "pass" <| fun _ ->
          Expect.sequenceEqual [1;2;3] [1;2;3] "Sequences actually equal"

        testCase "affine sequence pass" <| fun _ ->
          let bytes = Text.Encoding.UTF8.GetBytes("1\r\n2\r\n3\r\n")
          use stream = new IO.MemoryStream(bytes)
          let ofStreamByChunk chunkSize (stream: System.IO.Stream) =
            let buffer = Array.zeroCreate<byte> chunkSize
            seq { while stream.Length <> stream.Position do
                    let bytesRead = stream.Read(buffer, 0, chunkSize)
                    if bytesRead = 0 then ()
                    else yield buffer }

          let expected =
            [ Text.Encoding.UTF8.GetBytes("1\r\n")
              Text.Encoding.UTF8.GetBytes("2\r\n")
              Text.Encoding.UTF8.GetBytes("3\r\n") ]
            |> List.toSeq

          Expect.sequenceEqual (ofStreamByChunk 3 stream) expected "Sequences actually equal"

        testCase "fail - longer" (fun _ ->
          Expect.sequenceEqual [1;2;3] [1] "Deliberately failing"
        ) |> assertTestFails

        testCase "fail - shorter" (fun _ ->
          Expect.sequenceEqual [1] [1;2;3] "Deliberately failing"
        ) |> assertTestFails

        testCase "fail - not equal" (fun _ ->
          Expect.sequenceEqual [1;3] [1;2] "Deliberately failing"
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
          let! normal = testNormal c |> Impl.evalTestsSilent
          let! compexp = testCompExp c |> Impl.evalTestsSilent
          let normalTag = (snd normal.[0]).result.tag
          let compexpTag = (snd compexp.[0]).result.tag
          Expect.equal normalTag compexpTag "result"
        }
    ]
  ]

#if FSCHECK_TESTS

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

#endif

[<Tests>]
let asyncTests =
  testList "async" [

    testCaseAsync "simple" <| async {
      Expect.equal 1 1 "1=1"
    }

    testCaseAsync "let" <| async {
      let! n = async { return 1 }
      Expect.equal n 1 "n=1"
    }

    testCaseAsync "can fail" <| async {
      let! n = async { return 2 }
      Expect.equal n 1 "n=1"
    } |> assertTestFails

  ]

open System.Threading.Tasks

[<Tests>]
let taskTests =
  testList "task" [
    let withMemoryStreamTask f = task {
      use ms = new MemoryStream()
      do! f ms
    }

    yield! testFixtureTask withMemoryStreamTask [
      "can read",
        fun ms -> task { return ms.CanRead ==? true }
      "can write",
        fun ms -> task { return ms.CanWrite ==? true }
    ]

    testList "testCaseTask" [
      testCaseTask "simple" <| fun () ->
        task {
          Expect.equal 1 1 "1=1"
        }

      testCaseTask "let" <| fun () ->
        task {
          let! n = async { return 1 }
          Expect.equal n 1 "n=1"
        }

      testCaseTask "can fail" <| (fun () ->
        task {
          let! n = async { return 2 }
          Expect.equal n 1 "n=1"
        })
        |> assertTestFails
    ]

    testList "testTask" [
      testTask "simple" {
        do! Task.Delay 1
        Expect.equal 1 1 "1=1"
      }

      testTask "let" {
        let! n = Task.FromResult 23
        Expect.equal n 23 "n=23"
      }

      testTask "can fail" {
          let! n = Task.FromResult 2
          Expect.equal n 1 "n=1"
      } |> assertTestFails

      testTask "two" {
          let! n = Task.FromResult 2
          let! m = Task.FromResult (3*n)
          Expect.equal m 6 "m=6"
      }

      testTask "two can fail" {
          let! n = Task.FromResult 2
          let! m = Task.FromResult (3*n)
          Expect.equal m 7 "m=7"
      } |> assertTestFails

      testTask "two can fail middle" {
          let! n = Task.FromResult 2
          Expect.equal n 3 "n=3"
          let! m = Task.FromResult (3*n)
          Expect.equal m 6 "m=6"
      } |> assertTestFails

      testTask "inner skip" {
        skiptest "skipped"
      }
      testTask "can let! bind ValueTask<T>" {
        let expected = 5
        let valueTask = ValueTask.FromResult(expected)
        let! actual = valueTask
        Expect.equal expected actual "should be able to let! bind ValueTask<T>"
      }
      testTask "can do! bind ValueTask" {
        let valueTask = ValueTask.CompletedTask
        do! valueTask
      }
      testTask "can let! bind Async<T>" {
        let expected = 5
        let asyncValue = async.Return(expected)
        let! actual = asyncValue
        Expect.equal expected actual "should be able to let! bind ValueTask<T>"
      }
    ]
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

#if FSCHECK_TESTS
    testCase "popcount" (fun _ ->
      Expect.isFasterThan (fun () -> repeat10000 (popCount16 >> int) 987us)
                          (fun () -> repeat10000 (popCount >> int) 987us)
                          "popcount 16 faster than 32 fails"
      ) |> assertTestFailsWithMsgContaining "slower"
#endif
  ]

[<Tests>]
let close =
  testList "close" [

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

    testCase "less than easy" <| fun _ ->
      Expect.floatLessThanOrClose Accuracy.low -1.0 0.0 "less"

    testCase "not less than but close" <| fun _ ->
      Expect.floatLessThanOrClose Accuracy.low 0.000001 0.0 "close"

    testCase "not less than fails" (fun _ ->
      Expect.floatLessThanOrClose Accuracy.low 1.0 0.0 "fail"
    ) |> assertTestFails

    testCase "greater than easy" <| fun _ ->
      Expect.floatGreaterThanOrClose Accuracy.low 1.0 0.0 "greater"

    testCase "not greater than but close" <| fun _ ->
      Expect.floatGreaterThanOrClose Accuracy.low -0.000001 0.0 "close"

    testCase "not greater than fails" (fun _ ->
      Expect.floatGreaterThanOrClose Accuracy.low -1.0 0.0 "fail"
    ) |> assertTestFails

  ]

[<Tests>]
let stress =
  testList "stress testing" [

    let singleTest name =
      testList name [
        testCase "one" ignore
      ]

    let neverEndingTest =
      testList "never ending" [
        testAsync "never ending" {
          while true do
            do! Async.Sleep 10
        }
      ]

    let deadlockTest(name) =
      let lockOne = new obj()
      let lockTwo = new obj()
      testList name [
        testAsync "case A" {
          repeat100 (fun () ->
            lock lockOne (fun () ->
              Thread.Sleep 1
              lock lockTwo ignore
            )
          ) ()
        }
        testAsync "case B" {
          repeat100 (fun () ->
            lock lockTwo (fun () ->
              Thread.Sleep 1
              lock lockOne ignore
            )
          ) ()
        }
      ]

    let sequencedGroup() =
      testList "with other" [
        singleTest "first single test"
        testSequencedGroup "stop deadlock" (deadlockTest "first deadlock test")
        singleTest "second single test"
      ]

    let twoSequencedGroups() =
      testList "with other" [
        singleTest "first single test"
        testSequencedGroup "stop deadlock" (deadlockTest "first deadlock test")
        testSequencedGroup "stop deadlock other" (deadlockTest "second deadlock test")
        singleTest "second single test"
      ]

    yield testAsync "single" {
      let config = [
        Parallel_Workers 8
        Stress 0.002
        Stress_Timeout 0.2
        Printer TestPrinters.silent
        Verbosity Logging.LogLevel.Fatal
        No_Spinner
      ]
      Expect.equal (runTestsWithCLIArgs config [||] (singleTest "single test")) 0 "one"
    }

    yield testAsync "single with dot separator" {
      let config = [
        Parallel_Workers 8
        Stress 0.002
        Stress_Timeout 0.2
        Printer TestPrinters.silent
        Verbosity Logging.LogLevel.Fatal
        No_Spinner
        JoinWith "."
      ]
      Expect.equal (runTestsWithCLIArgs config [||] (singleTest "single test")) 0 "one"
    }

    yield testAsync "memory" {
      let config = [
        Parallel_Workers 8
        Stress 0.002
        Stress_Timeout 0.2
        Stress_Memory_Limit 0.001
        Printer TestPrinters.silent
        Verbosity Logging.LogLevel.Fatal
        No_Spinner
      ]
      Expect.equal (runTestsWithCLIArgs config [||] (singleTest "single test") &&& 4) 4 "memory"
    }

    yield testAsync "never ending" {
      let config = [
        Parallel_Workers 8
        Stress 0.1
        Stress_Timeout 0.2
        Printer TestPrinters.silent
        Verbosity Logging.LogLevel.Fatal
        No_Spinner
      ]
      Expect.equal (runTestsWithCLIArgs config [||] neverEndingTest) 8 "timeout"
    }

    yield testAsync "deadlock" {
      if Environment.ProcessorCount > 2 then
        let config = [
          Parallel_Workers 8
          Stress 0.1
          Stress_Timeout 0.2
          Printer TestPrinters.silent
          Verbosity Logging.LogLevel.Fatal
          No_Spinner
        ]
        Expect.equal (runTestsWithCLIArgs config [||] (deadlockTest "deadlock")) 8 "timeout"
    }

    yield testAsync "sequenced group" {
      let config = [
        Parallel_Workers 8
        Stress 0.1
        Stress_Timeout 0.2
        Printer TestPrinters.silent
        Verbosity Logging.LogLevel.Fatal
        No_Spinner
      ]
      Expect.equal (runTestsWithCLIArgs config [||] (sequencedGroup())) 0 "no timeout"
    }

    yield testAsync "two sequenced groups" {
      let config = [
        Parallel_Workers 8
        Stress 0.1
        Stress_Timeout 0.2
        Printer TestPrinters.silent
        Verbosity Logging.LogLevel.Fatal
        No_Spinner
      ]
      Expect.equal (runTestsWithCLIArgs config [||] (twoSequencedGroups())) 0 "no timeout"
    }

    yield testAsync "single sequenced" {
      let config = [
          Sequenced
          Stress 0.002
          Stress_Timeout 0.2
          Printer TestPrinters.silent
          Verbosity Logging.LogLevel.Fatal
          No_Spinner
      ]
      Expect.equal (runTestsWithCLIArgs config [||] (singleTest "single test")) 0 "one"
    }

    yield testAsync "memory sequenced" {
      let config = [
          Sequenced
          Stress 0.002
          Stress_Timeout 0.2
          Stress_Memory_Limit 0.001
          Printer TestPrinters.silent
          Verbosity Logging.LogLevel.Fatal
          No_Spinner
      ]
      Expect.equal (runTestsWithCLIArgs config [||] (singleTest "single test")) 4 "memory"
    }

    yield testAsync "never ending sequenced" {
      let config = [
          Sequenced
          Stress 0.1
          Stress_Timeout 0.2
          Printer TestPrinters.silent
          Verbosity Logging.LogLevel.Fatal
          No_Spinner
      ]
      Expect.equal (runTestsWithCLIArgs config [||] neverEndingTest) 8 "timeout"
    }

    yield testAsync "deadlock sequenced" {
      let config = [
          Sequenced
          Stress 0.1
          Stress_Timeout 0.2
          Printer TestPrinters.silent
          Verbosity Fatal
          No_Spinner
      ]
      Expect.equal (runTestsWithCLIArgs config [||] (deadlockTest "deadlock")) 0 "no deadlock"
    }
  ]

[<Tests>]
let cancel =
  testSequenced <| testList "cancel testing" [

    let cancelTestSync =
      testCaseWithCancel "sync cancel" (fun ct ->
        let mutable i = 200
        while not ct.IsCancellationRequested && i>0 do
          Thread.Sleep 10
          i <- i - 1
        if not ct.IsCancellationRequested then
          failwith "sync not cancelled"
      )

    let cancelTestAsync =
      testAsync "async cancel" {
        let mutable i = 200
        while i>0 do
          do! Async.Sleep 10
          i <- i - 1
        failwith "sync not cancelled"
      }

    yield! [
      false, cancelTestSync, "parallel false sync"
      false, cancelTestAsync, "parallel false async"
      true, cancelTestSync, "parallel true sync"
      true, cancelTestAsync, "parallel true async"
    ]
    |> List.map (fun (p,t,n) ->
      testAsync n {
        let config =
          { defaultConfig with
              runInParallel = p
              printer = TestPrinters.silent
              verbosity = Logging.LogLevel.Fatal
              noSpinner = true }
        use ct = new CancellationTokenSource()
        let! _ = Async.StartChild(async { do! Async.Sleep 50
                                          ct.Cancel() })
        let! results = evalTestsWithCancel ct.Token config t false
        results |> List.iter (fun (_,r) ->
          let d = int r.duration.TotalMilliseconds
          Expect.isLessThan d 1000 "cancel length"
        )
      }
    )
  ]

[<Tests>]
let theory =
  testList "theory testing" [
    testTheory "odd numbers" [1; 3; 5] <| fun x ->
      Expect.isTrue (x % 2 = 1) "should be odd"

    testTheory "empty strings" [""; null] <| fun x ->
      Expect.isTrue (System.String.IsNullOrEmpty(x)) "should be null or empty"

    testTheoryAsync "async even numbers" [2; 4; 6] <| fun x -> async {
      Expect.isTrue (x % 2 = 0) "should be even"
    }

    testTheoryAsync "async empty strings" [""; null] <| fun x -> async {
      Expect.isTrue (System.String.IsNullOrEmpty(x)) "should be null or empty"
    }

    testTheoryTask "task odd numbers" [1; 3; 5;] <| fun x -> task {
      Expect.isTrue (x % 2 = 1) "should be odd"
    }

    testTheoryTask "task empty strings" [""; null] <| fun x -> task {
      Expect.isTrue (System.String.IsNullOrEmpty(x)) "should be null or empty"
    }
  ]