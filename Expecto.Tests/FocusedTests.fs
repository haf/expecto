module Expecto.Focused
open Expecto
open Expecto.Tests
open Expecto.Impl

let synth = Expecto.Impl.sumTestResults
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
    testCase "pending" <| fun _ ->
        let result = evalSilent pendingTests |> synth
        result.passed ==? 2
        result.ignored ==? 5
    testCase "focused" <| fun _ ->
        let result = evalSilent focusedTests |> synth
        result.passed ==? 11
        result.ignored ==? 19
]

[<PTests>]
let ignoredTest = testCase "all focused tests/ignored by attribute" failing
