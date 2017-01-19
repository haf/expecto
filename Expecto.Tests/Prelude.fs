namespace Expecto
#nowarn "44"

open System

module Seq =
  let (|Empty|Cons|) l =
      if Seq.isEmpty l
          then Empty
          else Cons(Seq.head l, Seq.skip 1 l)

  let (|One|_|) l =
      match Seq.toList l with
      | [x] -> Some x
      | _ -> None

  let (|Two|_|) l =
      match Seq.toList l with
      | [x;y] -> Some(x,y)
      | _ -> None

[<AutoOpen>]
module TestHelpers =
  open Expecto
  open Expecto.Impl

  let rec private makeTest originalTest asyncTestFn =
    match originalTest with
    | TestCase (_,state)
    | TestList (_,state) ->
      TestCase(Async asyncTestFn, state)
    | TestLabel (label,_,state) ->
      TestLabel (label, TestCase(Async asyncTestFn, state), state)
    | Test.Sequenced test ->
      makeTest test asyncTestFn |> testSequenced

  let assertTestFails test =
    async {
      let! result = Impl.evalTestsSilent test
      match result with
      | [{ TestRunResult.result = TestResult.Ignored _ }] -> ()
      | [{ TestRunResult.result = TestResult.Failed _ }] -> ()
      | [x] -> failtestf "Should have failed, but was %A" x
      | _ -> failtestf "Should have one test to assert"
    } |> makeTest test

  let assertTestFailsWithMsgStarting (msg : string) test =
    async {
      let! result = Impl.evalTestsSilent test
      match result with
      | [{ TestRunResult.result = TestResult.Ignored _ }] -> ()
      | [{ TestRunResult.result = TestResult.Failed x }] ->
        let removeCR = x.Replace("\r","").Trim('\n')
        Expect.stringStarts removeCR msg "Test failure strings should equal"
      | [x] -> failtestf "Should have failed, but was %A" x
      | _ -> failtestf "Should have one test to assert"
    } |> makeTest test

  let assertTestFailsWithMsgContaining (msg : string) test =
    async {
      let! result = Impl.evalTestsSilent test
      match result with
      | [{ TestRunResult.result = TestResult.Ignored _ }] -> ()
      | [{ TestRunResult.result = TestResult.Failed x }] when x.Contains msg -> ()
      | [{ TestRunResult.result = TestResult.Failed x }] ->
        failtestf "Should have failed with message containing: \"%s\" but failed with \"%s\"" msg x
      | [x] -> failtestf "Should have failed, but was %A" x
      | _ -> failtestf "Should have one test to assert"
    } |> makeTest test

  open FsCheck

  let genLimitedTimeSpan =
      lazy (
          Arb.generate<TimeSpan>
          |> Gen.suchThat (fun t -> t.Days = 0)
      )

  let genTestRunResult =
    lazy (
      gen {
        let! name = Arb.generate<string>
        let! duration = genLimitedTimeSpan.Value

        return
          { TestRunResult.name = name
            location = SourceLocation.empty
            result = Passed
            duration = duration }
      }
    )

  let genTestResultCounts =
      lazy (
          gen {
              let! passed =  genTestRunResult.Value |> Gen.listOf
              let! ignored = genTestRunResult.Value |> Gen.listOf
              let! failed = genTestRunResult.Value |> Gen.listOf
              let! errored = genTestRunResult.Value |> Gen.listOf
              let! duration = genLimitedTimeSpan.Value
              return
                { TestResultSummary.passed = passed
                  ignored  = ignored
                  failed   = failed
                  errored  = errored
                  duration = duration }
          }
      )

  let shrinkTestResultCounts (c: TestResultSummary) : TestResultSummary seq =
      seq {
          for passed in Arb.shrink c.passed do
          for ignored in Arb.shrink c.ignored do
          for failed in Arb.shrink c.failed do
          for errored in Arb.shrink c.errored do
          for duration in Arb.shrink c.duration ->
          {
              TestResultSummary.passed = passed
              ignored = ignored
              failed = failed
              errored = errored
              duration = duration
          }
      }

  let arbTestResultCounts =
      lazy (
          Arb.fromGenShrink(genTestResultCounts.Value, shrinkTestResultCounts)
      )

  let twoTestResultCounts =
      lazy (
          Gen.two arbTestResultCounts.Value.Generator |> Arb.fromGen
      )

  let inline repeat10 f a =
    let mutable v = f a
    v <- f a
    v <- f a
    v <- f a
    v <- f a
    v <- f a
    v <- f a
    v <- f a
    v <- f a
    v <- f a
    v
  let inline repeat100 f a = repeat10 (repeat10 f) a
  let inline repeat1000 f a = repeat10 (repeat100 f) a
  let inline repeat10000 f a = repeat10 (repeat1000 f) a
  let inline repeat100000 f a = repeat10 (repeat10000 f) a
  let inline repeat1000000 f a = repeat10 (repeat100000 f) a