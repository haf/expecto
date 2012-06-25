namespace Fuchu

open System

module Seq = 
    let (|Empty|Cons|) l = 
        if Seq.isEmpty l
            then Empty
            else Cons(Seq.head l, Seq.skip 1 l)

    let (|One|_|) l = 
        let a = Seq.toList l
        if a.Length = 1
            then Some a.[0]
            else None

    let (|Two|_|) l = 
        let a = Seq.toList l
        if a.Length = 2
            then Some (a.[0], a.[1])
            else None

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
