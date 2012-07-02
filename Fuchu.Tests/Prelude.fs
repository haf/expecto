namespace Fuchu

open System
open FSharpx

[<AutoOpen>]
module Assert = 
    let inline assertNone preface =
        let preface = 
            if String.IsNullOrEmpty preface
                then ""
                else preface + "\n"
        
        function
        | Some x -> failtestf "%sExpected None, actual Some (%A)" preface x
        | _ -> ()

    let inline assertEqual preface =
        let preface = 
            if String.IsNullOrEmpty preface
                then ""
                else preface + "\n"
        fun expected actual ->
            if expected <> actual then
                failtestf "%sExpected: %A\nActual: %A" preface expected actual

    let inline (==?) actual expected =
        assertEqual null expected actual

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
    let internal stringMatch f expected e =
        if e = null && expected = null then
            Some()
        elif e = null || expected = null then
            None
        else
            f e expected |> Option.fromBool

    let (|StartsWith|_|) =
        stringMatch (fun (s: string) -> s.StartsWith)

    let (|Contains|_|) =
        stringMatch (fun (s: string) -> s.Contains)

[<AutoOpen>]
module TestHelpers = 
    open Fuchu
    open Fuchu.Impl

    let evalSilent = eval TestPrinters.Default Seq.map

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
                let! time = genLimitedTimeSpan.Value
                return {
                    TestResultCounts.Passed = passed
                    Ignored = ignored
                    Failed = failed
                    Errored = errored
                    Time = time
                }
            }
        )

    let shrinkTestResultCounts (c: TestResultCounts) : TestResultCounts seq = 
        seq {
            for passed in Arb.shrink c.Passed do
            for ignored in Arb.shrink c.Ignored do
            for failed in Arb.shrink c.Failed do
            for errored in Arb.shrink c.Errored do
            for time in Arb.shrink c.Time ->
            {
                TestResultCounts.Passed = passed
                Ignored = ignored
                Failed = failed
                Errored = errored
                Time = time
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
