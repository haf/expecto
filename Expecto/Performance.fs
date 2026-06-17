namespace Expecto

open System
open Expecto.Logging
open Expecto.Logging.Message
open FSharp.Core.LanguagePrimitives

type Accuracy<'abs, 'rel> = { absolute: 'abs; relative: 'rel }

type Accuracy<[<Measure>] 'u> = Accuracy<float<'u>, float>
type Accuracy = Accuracy<1>
type Accuracy32<[<Measure>] 'u> = Accuracy<float32<'u>, float32>
type Accuracy32 = Accuracy32<1>

module Accuracy =
  let inline areCloseLhs a b : ^a = abs(a-b)
  let inline areCloseRhs (m: Accuracy<'a,_>) (a: 'a) (b: 'a) : 'a = m.absolute + m.relative * max (abs a) (abs b)
  let inline areClose (m: Accuracy<'a,_>) (a: 'a) (b: 'a) : bool = areCloseLhs a b <= areCloseRhs m a b

  let low<[<Measure>] 'u> : Accuracy<'u>        = { absolute = FloatWithMeasure 1e-6; relative = 1e-3 }
  let medium<[<Measure>] 'u> : Accuracy<'u>     = { absolute = FloatWithMeasure 1e-8; relative = 1e-5 }
  let mediumf<[<Measure>] 'u> : Accuracy32<'u>   = { absolute = Float32WithMeasure 1e-8f; relative = 1e-5f }
  let high<[<Measure>] 'u> : Accuracy<'u>       = { absolute = FloatWithMeasure 1e-10; relative = 1e-7 }
  let highf<[<Measure>] 'u> : Accuracy32<'u>     = { absolute = Float32WithMeasure 1e-10f; relative = 1e-7f }
  let veryHigh<[<Measure>] 'u> : Accuracy<'u>   = { absolute = FloatWithMeasure 1e-12; relative = 1e-9 }
  let veryHighf<[<Measure>] 'u> : Accuracy32<'u> = { absolute = Float32WithMeasure 1e-12f; relative = 1e-9f }

module Accuracy32 =
  open Accuracy

  let low<[<Measure>] 'u> : Accuracy32<'u>      = { absolute = Float32WithMeasure 1e-6f; relative = 1e-3f }
  let medium<[<Measure>] 'u> : Accuracy32<'u>   = { absolute = Float32WithMeasure 1e-8f; relative = 1e-5f }
  let high<[<Measure>] 'u> : Accuracy32<'u>     = { absolute = Float32WithMeasure 1e-10f; relative = 1e-7f }
  let veryHigh<[<Measure>] 'u> : Accuracy32<'u> = { absolute = Float32WithMeasure 1e-12f; relative = 1e-9f }

module Performance =
  open Statistics
  open System.Diagnostics

  let inline private measureStatistics metric relativeError f =
    Seq.initInfinite (fun _ -> metric f) |> sampleStatistics
    |> Seq.find (fun s -> s.meanStandardError<=relativeError*s.mean)

  type 'a CompareResult =
    | ResultNotTheSame of result1:'a * result2:'a
    | MetricTooShort of sMax:SampleStatistics * machineResolution:SampleStatistics
    | MetricLessThan of s1:SampleStatistics * s2:SampleStatistics
    | MetricMoreThan of s1:SampleStatistics * s2:SampleStatistics
    | MetricEqual of s1:SampleStatistics * s2:SampleStatistics
  let inline private measureCompare metric f1 f2 =
    let r1 = f1 id
    let r2 = f2 id
    if r1 <> r2 then ResultNotTheSame (r1,r2)
    else
      let stats f = Seq.initInfinite (fun _ -> metric f) |> sampleStatistics

      let machinePrecisionFunction = (fun m -> m (fun () -> Unchecked.defaultof<_>) ())
      stats machinePrecisionFunction |> Seq.item 2 |> ignore

      let precision =
        stats machinePrecisionFunction
        |> Seq.skip 5
        |> Seq.head

      Seq.zip (stats f1) (stats f2)
      |> Seq.skip 10
      |> Seq.pick (fun (s1,s2) ->

        let inline areCloseEnough() =
          let meanPointFivePercent = (s1.mean + s2.mean) * 0.5 * 0.005
          abs(s1.mean-s2.mean) < meanPointFivePercent
          &&
            let differenceSD = sqrt(s1.variance/float s1.N+s2.variance/float s2.N)
            differenceSD * normInv99_995 < meanPointFivePercent

        if max s1.mean s2.mean < precision.mean * 5.0 then
          MetricTooShort ((if s1.mean<s2.mean then s2 else s1),precision) |> Some
        elif areCloseEnough() then MetricEqual (s1,s2) |> Some
        else welchStatistic s1 s2 |> welchTest
             |> Option.map (function
               | 0 -> MetricEqual (s1,s2)
               | 1 -> MetricMoreThan (s1,s2)
               | _ -> MetricLessThan (s1,s2)
               )
          )

  type Measurer<'a,'b> = ('a->'b) -> ('a->'b)

  let inline private measureMetric startMetric endMetric (f:Measurer<_,_>->_) =
    GC.Collect()
    GC.WaitForPendingFinalizers()
    GC.Collect()
    let mutable total = LanguagePrimitives.GenericZero
    let measurer toMeasure args =
      let s = startMetric()
      let ret = toMeasure args
      let m = endMetric s
      total<-total+m
      ret
    f measurer |> ignore
    total

  let private timeMetric f =
    measureMetric Stopwatch.GetTimestamp (fun ts -> Stopwatch.GetTimestamp() - ts) f

  let private toMilliseconds = scale (1000.0/float Stopwatch.Frequency)

  /// Time statistics for a given function accurate to a mean standard error of 0.1%.
  let timeStatistics f = measureStatistics timeMetric 0.001 f |> toMilliseconds

  /// Time comparison for two given functions to a 99.99% confidence level.
  let timeCompare (f1:Measurer<_,_> -> _) (f2:Measurer<_,_> -> _) =
    match measureCompare timeMetric f1 f2 with
    | ResultNotTheSame (r1,r2) -> ResultNotTheSame (r1,r2)
    | MetricTooShort (s,p) -> MetricTooShort (toMilliseconds s,toMilliseconds p)
    | MetricEqual (s1,s2) -> MetricEqual (toMilliseconds s1,toMilliseconds s2)
    | MetricMoreThan (s1,s2) -> MetricMoreThan (toMilliseconds s1,toMilliseconds s2)
    | MetricLessThan (s1,s2) -> MetricLessThan (toMilliseconds s1,toMilliseconds s2)

  /// Find the int input of the function f between lo and hi that gives the
  /// fastest execution time. Statistical test to 99.99% confidence level
  /// using a trisect search.
  let findFastest (f:int->'a) lo hi : int =

    let search (f:int*int->float*float) lo hi =
      let rec search lo flo hi fhi =
        Impl.logger.log Info (
              eventX "findFastest: seaching range {lo} {hi}"
              >> setField "lo" (string lo)
              >> setField "hi" (string hi))
        |> Async.StartImmediate

        if lo=hi then lo
        elif lo+1=hi then
          let flo,fhi = f(lo,hi)
          if flo<fhi then lo else hi
        elif lo+2=hi then
          if flo<fhi then
            let flo,fm = f(lo,lo+1)
            if flo<fm then lo
            else
              let fm,fhi = f(lo+1,hi)
              if fm<fhi then lo+1 else hi
          else
            let fm,fhi = f(lo+1,hi)
            if fhi<fm then hi
            else
              let flo,fm = f(lo,lo+1)
              if flo<fm then lo else lo+1
        else
          let a,b =
            if lo+3=hi then lo+1,hi-1
            else
              let m = float(hi-lo)/3.0
              let a = float lo + m |> round |> int
              let b = float hi - m |> round |> int
              a,b
          let fa,fb = f(a,b)
          if fa<fb then search lo flo b fb else search a fa hi fhi
      let flo,fhi = f(lo,hi)
      search lo flo hi fhi

    let f (a,b) =
      let toString (s:SampleStatistics) =
        sprintf "%.4f \u00B1 %.4f ms" s.mean s.meanStandardError
      let r = timeCompare (fun measurer -> measurer (fun () -> f a) ())
                          (fun measurer -> measurer (fun () -> f b) ())
      match r with
      | ResultNotTheSame (r1,r2) ->
        Tests.failtestNoStackf "Expected results to be the same. f(%i)=%A f(%i)=%A"
          a r1 b r2
      | MetricTooShort (s,p) ->
        Tests.failtestNoStackf
          "Expected metric (%s) to be much longer than the machine resolution (%s)."
          (toString s) (toString p)
      | MetricEqual (s1,s2) -> s1.mean,s2.mean
      | MetricMoreThan (s1,s2) -> s1.mean,s2.mean
      | MetricLessThan (s1,s2) -> s1.mean,s2.mean

    search f lo hi
