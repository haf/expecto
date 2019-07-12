module Expecto.Hopac.Tests

open Hopac
open Expecto
open Expecto.Impl
open System
open System.Threading.Tasks

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
let testCaseJobTests =
  testList "testCaseJob" [

    testCaseJob "simple job" <| job {
      Expect.equal 1 1 "1=1"
    }

    testCaseJob "let! job" <| job {
      let! n = job { return 1 }
      Expect.equal 1 n "1=n"
    }

    testCaseJob "job can fail" <| job {
      let! n = job { return 2 }
      Expect.equal 1 n "1=n"
    } |> assertTestFails

  ]

let emptyDisposeable = {
  new IDisposable with
    member __.Dispose () = ()
}

let oneShotObservable value = {
  new IObservable<_> with
    member __.Subscribe(obs : IObserver<_>) =
      obs.OnNext value
      obs.OnCompleted ()
      emptyDisposeable
}

[<Tests>]
let testJobTests =
  testList "testJob" [

    testJob "let! observable"  {
      let! n = oneShotObservable 1
      Expect.equal 1 n "1=n"
    }

    testJob "let! job"  {
      let! n = job { return 1 }
      Expect.equal 1 n "1=n"
    }

    testJob "let! async"  {
      let! n = async { return 1 }
      Expect.equal 1 n "1=n"
    }

    testJob "let! task"  {
      let! n = Task.FromResult 1
      Expect.equal 1 n "1=n"
    }
  ]
