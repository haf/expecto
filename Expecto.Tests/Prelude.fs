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
        if a = null && a = null then
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

    let evalSilent = eval TestPrinters.Default Seq.map

    let inline assertTestFails test =
        let test = TestCase test
        match evalSilent test with
        | [{ TestRunResult.result = TestResult.Failed _ }] -> ()
        | x -> failtestf "Should have failed, but was %A" x

    let inline assertTestFailsWithMsg (msg : string) test =
        let test = TestCase test
        match evalSilent test with
        | [{ TestRunResult.result = TestResult.Failed x }] when String.Equals(x.Replace("\n", ""),msg.Replace("\n", "")) -> ()
        | [{ TestRunResult.result = TestResult.Failed x }] -> failtestf "Shold have failed with message: \"%s\" but failed with \"%s\"" msg x
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
                let! passed = Arb.generate<int>
                let! ignored = Arb.generate<int>
                let! failed = Arb.generate<int>
                let! errored = Arb.generate<int>
                let! duration = genLimitedTimeSpan.Value
                return
                  { TestResultCounts.passed = passed
                    ignored  = ignored
                    failed   = failed
                    errored  = errored
                    duration = duration }
            }
        )

    let shrinkTestResultCounts (c: TestResultCounts) : TestResultCounts seq =
        seq {
            for passed in Arb.shrink c.passed do
            for ignored in Arb.shrink c.ignored do
            for failed in Arb.shrink c.failed do
            for errored in Arb.shrink c.errored do
            for duration in Arb.shrink c.duration ->
            {
                TestResultCounts.passed = passed
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
