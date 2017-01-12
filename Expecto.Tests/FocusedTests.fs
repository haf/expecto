module Expecto.Focused
open Expecto
open Expecto.Tests
open Expecto.Impl

let synth = Impl.sumTestResults
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
  ]

let focusedTests =
  testList "focused tests" [
    test "normal" { 1 ==? 2 }
    ptest "ignored" { 1 ==? 2 }
    ftest "focused" { () }
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
      let! result =
        Impl.evalSilentAsync pendingTests
        |> Async.map synth

      result.passed.Length ==? 2
      result.ignored.Length ==? 5
    }
    testCaseAsync "focused" <| async {
      let! result =
        Impl.evalSilentAsync focusedTests
        |> Async.map synth

      result.passed.Length ==? 11
      result.ignored.Length ==? 19
    }
    testCase "can detect focused test" <| fun _ ->
      let localList =
        testList "local" [
          ftestList "focused" [
            testCase "test" (fun () -> ())
          ]
        ]

      // check if we can fail on focused tests
      if runTests { defaultConfig with failOnFocusedTests = true } localList <> 1 then
        failwith "focused test check didn't fail"
    testCase "can run if no focused test was found" <| fun _ ->
      let localList =
        testList "local" [
          testList "focused" [
            testCase "test" (fun () -> ())
          ]
        ]

      // check if we pass if no focused tests exist
      if runTests { defaultConfig with failOnFocusedTests = true } localList <> 0 then
        failwith "focused test check didn't fail"
]

[<PTests>]
let ignoredTest = testCase "all focused tests/ignored by attribute" failing
