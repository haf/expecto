module Expecto.Hopac.Tests

open Hopac
open Expecto
open Expecto.Impl

let rec private makeTest originalTest asyncTestFn =
  match originalTest with
  | TestCase (_,state)
  | TestList (_,state) ->
    TestCase(Async asyncTestFn, state)
  | TestLabel (label,_,state) ->
    TestLabel (label, TestCase(Async asyncTestFn, state), state)
  | Test.Sequenced (sequenced,test) ->
    Test.Sequenced (sequenced,makeTest test asyncTestFn)
let assertTestFails test =
  async {
    let! result = Impl.evalTestsSilent test
    match result with
    | [(_,{ result = TestResult.Ignored _ })] -> ()
    | [(_,{ result = TestResult.Failed _ })] -> ()
    | [(_,{ result = TestResult.Error _ })] -> ()
    | [x] -> failtestf "Should have failed, but was %A" x
    | _ -> failtestf "Should have one test to assert"
  } |> makeTest test

[<Tests>]
let tests =
  testList "all" [
    testJob "using job computation expression" {
      Expect.equal 4 (2+2) "2+2"
    }

    testJob "using job computation expression with bind" {
      let! x = job { return 4 }
      Expect.equal x (2+2) "2+2"
    }

  ]

[<Tests>]
let jobTests =
  testList "job" [

    testCaseJob "simple job" <| job {
      Expect.equal 1 1 "1=1"
    }

    testCaseJob "let job" <| job {
      let! n = job { return 1 }
      Expect.equal 1 n "1=n"
    }

    testCaseJob "job can fail" <| job {
      let! n = job { return 2 }
      Expect.equal 1 n "1=n"
    } |> assertTestFails

  ]

