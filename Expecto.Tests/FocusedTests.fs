module Expecto.Focused

open System
open Expecto
open Expecto.Tests
open Expecto.Impl
open Expecto.Logging

let failing = fun _ -> 1 ==? 2
let working = ignore

let pendingTests =
  testList "pending tests" [
    ptestList "ignored" [
      ptestCase "ignored" failing
      testCase "normal" failing
      test "normal" { 1 ==? 2 }
      ptest "ignored" { 1 ==? 2 }
    ]
    testCase "normal" working
    test "normal" { () }
    ptest "ignored" { 1 ==? 2 }
    testTheory "normal" [()] working
    ptestTheory "ignored" [()] failing
  ]

let focusedTests =
  testList "focused tests" [
    test "normal" { 1 ==? 2 }
    ptest "ignored" { 1 ==? 2 }
    ftest "focused" { () }
    testTheory "normal" [()] failing
    ptestTheory "ignored" [()] failing
    ftestTheory "focused" [()] working
    ptestCase "ignored" failing
    testCase "normal" failing
    ftestCase "focused" working
    ftestList "focused" [
      ptestCase "ignored" failing
      testCase "normal" working
      ftestCase "focused" working
      testList "normal" [
        test "normal" { () }
        ptest "ignored" { 1 ==? 2 }
        ftest "focused" { () }
        ptestCase "ignored" failing
        testCase "normal" working
        ftestCase "focused" working ]
      ptestList "ignored" [
        ftest "focused" { 1 ==? 2 }
        ptestCase "ignored" failing
        testCase "normal" failing
        ftestCase "focused" failing]
      ftestList "focused" [
        ptest "ignored" { 1 ==? 2 }
        ptestCase "ignored" failing
        testCase "normal" working
        ftestCase "focused" working ]
    ]
    testList "normal" [
      ptestCase "ignored" failing
      testCase "normal" failing
      ftestCase "focused" working ]
    ptestList "ignored" [
      ftest "focused" { 1 ==? 2 }
      ptestCase "ignored" failing
      testCase "normal" failing
      ftestCase "focused" failing]
  ]

[<Tests>]
let all =
  testList "all focused tests" [
    testCaseAsync "pending" <| async {
      let! resultList = Impl.evalTestsSilent pendingTests
      let result = { results = resultList
                     duration = TimeSpan.Zero
                     maxMemory = 0L
                     memoryLimit = 0L
                     timedOut = [] }

      Seq.length result.passed ==? 3
      Seq.length result.ignored ==? 6
    }
    testCaseAsync "focused" <| async {
      let! result = Impl.evalTestsSilent focusedTests
      let result = { results = result
                     duration = TimeSpan.Zero
                     maxMemory = 0L
                     memoryLimit = 0L
                     timedOut = [] }

      Seq.length result.passed ==? 12
      Seq.length result.ignored ==? 21
    }
    testCase "can detect focused test" <| fun _ ->
      let localList =
        testList "local" [
          ftestList "focused" [
            testCase "test" (fun () -> ())
          ]
        ]

      // check if we can fail on focused tests
      if runTestsWithCLIArgs
          [
            Fail_On_Focused_Tests
            Printer TestPrinters.silent
            Verbosity Fatal
            No_Spinner
          ] [||] localList
         <> 1 then
        failwith "focused test check didn't fail"
    testCase "can run if no focused test was found" <| fun _ ->
      let localList =
        testList "local" [
          testList "focused" [
            testCase "test" (fun () -> ())
          ]
        ]

      // check if we pass if no focused tests exist
      if runTestsWithCLIArgs
          [
            Fail_On_Focused_Tests
            Printer TestPrinters.silent
            Verbosity Fatal
            No_Spinner
          ] [||] localList
         <> 0 then
        failwith "focused test check didn't fail"
]

[<PTests>]
let ignoredTest = testCase "all focused tests/ignored by attribute" failing

[<Tests>]
let configTests =

  let dummyTests =
    testList "dummy" [
      testCase "hi" ignore
      testCase "2nd" ignore |> testSequenced
    ]

  let nonDuplicateNamesTests =
    testList "same root" [
      testList "path one" [
        testCase "same name" ignore
      ]
      testList "path two" [
        testCase "same name" ignore
      ]
    ]

  let duplicateNamesTests =
    testList "same path" [
      testCase "same name" ignore
      testCase "same name" ignore
    ]

  let silentConfig = [
    Verbosity Fatal
    Printer TestPrinters.silent
    No_Spinner
   ]

  testList "config tests" [
    testCase "parallel config works" <| fun _ ->
      let retCode = runTestsWithCLIArgs (Sequenced::silentConfig) [||] dummyTests
      Expect.equal retCode 0 "return code zero"

    testCase "parallel config overrides" <| fun _ ->
      let retCode = runTestsWithCLIArgs (Parallel_Workers 8::Sequenced::silentConfig) [||]
                      dummyTests
      Expect.equal retCode 0 "return code zero"

    testCase "none duplicate names are fine" <| fun _ ->
      let retCode = runTestsWithCLIArgs silentConfig [||] nonDuplicateNamesTests
      Expect.equal retCode 0 "return code zero"

    testCase "duplicate names fails by default" <| fun _ ->
      let retCode = runTestsWithCLIArgs silentConfig [||] duplicateNamesTests
      Expect.equal retCode 1 "return code one"

    testCase "duplicate names ok if set in config" <| fun _ ->
      let retCode = runTestsWithCLIArgs (Allow_Duplicate_Names::silentConfig) [||]
                      duplicateNamesTests
      Expect.equal retCode 0 "return code zero"
  ]