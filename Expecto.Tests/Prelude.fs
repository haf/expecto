namespace Expecto
#nowarn "44"

open System
open FSharpx

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

module String =
    let internal nullBool2 f a b =
        if a = null && a = null then // TODO: Looks like a bug here, module seems unused
            true
        elif a = null || b = null then
            false
        else
            f b a

    let internal nullOption2 f a b =
        nullBool2 f a b |> Option.ofBool

    let (|StartsWith|_|) =
        nullOption2 (fun (s: string) -> s.StartsWith)

    let (|Contains|_|) =
        nullOption2 (fun (s: string) -> s.Contains)

[<AutoOpen>]
module TestHelpers =
  open Expecto
  open Expecto.Impl

  let evalSilent = eval TestPrinters.silent List.map

  let inline assertTestFails test =
    let test = TestCase test
    match evalSilent test with
    | [{ TestRunResult.result = TestResult.Failed _ }] -> ()
    | x -> failtestf "Should have failed, but was %A" x

  let inline assertTestFailsWithMsg (msg : string) test =
    let test = TestCase test
    match evalSilent test with
    | [{ TestRunResult.result = TestResult.Failed x }] ->
      let trimmed = x.Trim('\n')
      Expect.equal trimmed msg "Test failure strings should equal"
    | x ->
      failtestf "Should have failed, but was %A" x

  let inline assertTestFailsWithMsgContaining (msg : string) test =
    let test = TestCase test
    match evalSilent test with
    | [{ TestRunResult.result = TestResult.Failed x }] when x.Contains msg -> ()
    | [{ TestRunResult.result = TestResult.Failed x }] ->
      failtestf "Should have failed with message containing: \"%s\" but failed with \"%s\"" msg x
    | x -> failtestf "Should have failed, but was %A" x

  open FsCheck

  let genLimitedTimeSpan =
      lazy (
          Arb.generate<TimeSpan>
          |> Gen.suchThat (fun t -> t.Days = 0)
      )

  let genTestResultCounts =
      lazy (
          gen {
              let! passed = Arb.generate<string list>
              let! ignored = Arb.generate<string list>
              let! failed = Arb.generate<string list>
              let! errored = Arb.generate<string list>
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