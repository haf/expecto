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

    // These generators will probably be included in the next release of FsCheck

    open FsCheck

    let genInt64 = 
        lazy (
            let inline uuint64 x = uint64 (uint32 x)
            Gen.two Arb.generate<int>
            |> Gen.map (fun (h,l) -> int64 ((uuint64 h <<< 32) ||| uuint64 l))
        )

    let arbTimeSpan = 
        lazy (
            let genTimeSpan = genInt64.Value |> Gen.map (fun ticks -> TimeSpan ticks)
            let shrink (t: TimeSpan) = 
                if t.Days > 0 then
                    seq { yield TimeSpan(0, t.Hours, t.Minutes, t.Seconds, t.Milliseconds) }
                elif t.Hours > 0 then
                    seq { yield TimeSpan(0, 0, t.Minutes, t.Seconds, t.Milliseconds) }
                elif t.Minutes > 0 then
                    seq { yield TimeSpan(0, 0, 0, t.Seconds, t.Milliseconds) }
                elif t.Seconds > 0 then
                    seq { yield TimeSpan(0, 0, 0, 0, t.Milliseconds) }
                elif t.Milliseconds > 0 then
                    seq { yield TimeSpan(0L) }
                else
                    Seq.empty
            Arb.fromGenShrink (genTimeSpan, shrink)
        )

    type ArbRegistrations = 
        static member Int64() = Arb.fromGen genInt64.Value
        static member TimeSpan() = arbTimeSpan.Value

    Arb.register<ArbRegistrations>() |> ignore