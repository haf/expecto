namespace Expecto

open System

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

      let s1,s2,precision =
        let seq = Seq.zip3 (stats f1) (stats f2) (stats (fun m -> m (fun () -> Unchecked.defaultof<_>) ()))
        Seq.item 2 seq |> ignore
        Seq.item 5 seq

      if max s1.mean s2.mean < precision.mean * 5.0 then
        MetricTooShort ((if s1.mean<s2.mean then s2 else s1),precision)
      else
        Seq.zip (stats f1) (stats f2)
        |> Seq.pick (fun (s1,s2) ->

          let inline areCloseEnough() =
            let s1,s2 = if s1.mean>s2.mean then s1,s2 else s2,s1
            let numberOfSD = 2.325 // Equivalent to 99.99% confidence level
            s1.mean + numberOfSD * s1.meanStandardError - (s2.mean - numberOfSD * s2.meanStandardError) < (s1.mean + s2.mean) * 0.5 * 0.005

          if areCloseEnough() then MetricEqual (s1,s2) |> Some
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
        if lo=hi then lo
        elif lo+1=hi then if flo<fhi then lo else hi
        elif lo+2=hi then
            if flo<fhi then
                let flo,fm = f(lo,lo+1)
                if flo<fm then lo else lo+1
            else
                let fm,fhi = f(lo+1,hi)
                if fm<fhi then lo+1 else hi
        else
          let a,b =
            if lo+3=hi then lo+1,hi-1
            else
              let m = float(hi-lo)/3.0
              let a = float lo + m |> (+) 0.5 |> int
              let b = float hi - m |> (+) 0.5 |> int
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
