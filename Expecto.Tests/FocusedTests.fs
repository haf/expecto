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
      ]
      testCase "normal" working
    ]

  let focusedTests =
    testList "focused tests" [
      ptestCase "ignored" failing
      testCase "normal" failing
      ftestCase "focused" working
      ftestList "focused" [
        ptestCase "ignored" failing
        testCase "normal" working
        ftestCase "focused" working
        testList "normal" [
          ptestCase "ignored" failing
          testCase "normal" working
          ftestCase "focused" working ]
        ptestList "ignored" [
          ptestCase "ignored" failing
          testCase "normal" failing
          ftestCase "focused" failing]
        ftestList "focused" [
          ptestCase "ignored" failing
          testCase "normal" working
          ftestCase "focused" working ]
      ]
      testList "normal" [
        ptestCase "ignored" failing
        testCase "normal" failing
        ftestCase "focused" working ]
      ptestList "ignored" [
        ptestCase "ignored" failing
        testCase "normal" failing
        ftestCase "focused" failing]
    ]

  [<Tests>]
  let all = testList "all focused tests" [
                                          testCase "pending" <| fun _ ->
                                              let result = evalSilent pendingTests |> synth
                                              result.passed ==? 1
                                              result.ignored ==? 2
                                          testCase "focused" <| fun _ ->
                                              let result = evalSilent focusedTests |> synth
                                              result.passed ==? 8
                                              result.ignored ==? 13
  ]