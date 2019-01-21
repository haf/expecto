namespace Expecto

open System
open System.Collections.Generic

type SortedList() =
  let l = LinkedList<int64>()
  member __.Add(i:int64) =
    let rec add (node:LinkedListNode<int64>) =
      if isNull node then l.AddLast i |> ignore
      elif node.Value > i then l.AddBefore(node, i) |> ignore
      else add node.Next
    add l.First
  member __.ToSeq() =
      l :> int64 seq

type SampleStatistics =
  { N:int
    mean:float
    variance:float }
  member s.standardDeviation = sqrt s.variance
  member s.meanStandardError = sqrt(s.variance/float s.N)

module internal Statistics =
  let inline private sqr x = x*x


  /// Inverse normal for 99.995%. Two-tailed 99.99% Z-Score.
  [<Literal>]
  let normInv99_995 = 3.890591886 // =NORM.S.INV(0.99995)

  let initialIntermediateStatistics =
    0,0.0,0.0

  let inline updateIntermediateStatistics (n,m,s) x =
    let m' = m+(x-m)/float(n+1)
    let s'= s+(x-m)*(x-m')
    // Reject as outlier if more than 4 s.d.
    if (x-m')*(x-m') * float n > (normInv99_995 * normInv99_995) * s' then printfn "Removed %A from %A = %f" (n+1,x) (m',sqrt(s'/float(n-1))) (abs(x-m')/sqrt(s'/float n)); n,m,s
    else n+1,m',s'

  let inline intermediateToSampleStatistics (n,m,s) =
    {N=n;mean=m;variance=s/float(n-1)}

  /// Online statistics sequence for a given sample sequence.
  let inline sampleStatistics s =
    Seq.map float s
    |> Seq.scan updateIntermediateStatistics initialIntermediateStatistics
    |> Seq.map intermediateToSampleStatistics

  /// Scale the statistics for the given underlying random variable change of scale.
  let scale f s = {s with mean=s.mean*f;variance=s.variance*sqr f}

  /// Student's t-distribution inverse for the 00.01% probability by degrees of freedom.
  let private tInv01 = [|6366.198;99.992;28.000;15.544;11.178;9.082;7.885;7.120;6.594;6.211;5.921;5.694;5.513;5.363;5.239;5.134;5.044;4.966;4.897;4.837;4.784;4.736;4.693;4.654;4.619;4.587;4.558;4.530;4.506;4.482;4.461;4.441;4.422;4.405;4.389;4.374;4.359;4.346;4.333;4.321;4.309;4.298;4.288;4.278;4.269;4.260;4.252;4.243;4.236;4.228;4.221;4.214;4.208;4.202;4.196;4.190;4.184;4.179;4.174;4.169;4.164;4.159;4.155;4.150;4.146;4.142;4.138;4.134;4.130;4.127;4.123;4.120;4.117;4.113;4.110;4.107;4.104;4.101;4.099;4.096;4.093;4.091;4.088;4.086;4.083;4.081;4.079;4.076;4.074;4.072;4.070;4.068;4.066;4.064;4.062;4.060;4.059;4.057;4.055;4.053|]
  /// Student's t-distribution inverse for the 99.99% probability by degrees of freedom.
  let private tInv99 = [|1.571E-04;1.414E-04;1.360E-04;1.333E-04;1.317E-04;1.306E-04;1.299E-04;1.293E-04;1.289E-04;1.285E-04;1.282E-04;1.280E-04;1.278E-04;1.276E-04;1.274E-04;1.273E-04;1.272E-04;1.271E-04;1.270E-04;1.269E-04;1.268E-04;1.268E-04;1.267E-04;1.266E-04;1.266E-04;1.265E-04;1.265E-04;1.265E-04;1.264E-04;1.264E-04;1.263E-04;1.263E-04;1.263E-04;1.263E-04;1.262E-04;1.262E-04;1.262E-04;1.262E-04;1.261E-04;1.261E-04;1.261E-04;1.261E-04;1.261E-04;1.260E-04;1.260E-04;1.260E-04;1.260E-04;1.260E-04;1.260E-04;1.260E-04;1.259E-04;1.259E-04;1.259E-04;1.259E-04;1.259E-04;1.259E-04;1.259E-04;1.259E-04;1.259E-04;1.259E-04;1.258E-04;1.258E-04;1.258E-04;1.258E-04;1.258E-04;1.258E-04;1.258E-04;1.258E-04;1.258E-04;1.258E-04;1.258E-04;1.258E-04;1.258E-04;1.258E-04;1.257E-04;1.257E-04;1.257E-04;1.257E-04;1.257E-04;1.257E-04;1.257E-04;1.257E-04;1.257E-04;1.257E-04;1.257E-04;1.257E-04;1.257E-04;1.257E-04;1.257E-04;1.257E-04;1.257E-04;1.257E-04;1.257E-04;1.257E-04;1.257E-04;1.257E-04;1.257E-04;1.257E-04;1.256E-04;1.256E-04|]

  type WelchStatistic = {T:float;DF:int}

  /// Welch's t-test statistic for two given sample statistics.
  let welchStatistic s1 s2 =
    let f1 = s1.variance/float s1.N
    let f2 = s2.variance/float s2.N
    { T = (s1.mean-s2.mean)/sqrt(f1+f2)
      DF= if f1=0.0 && f2=0.0 then 1
          else sqr(f1+f2)/(sqr f1/float(s1.N-1)+sqr f2/float(s2.N-1)) |> int
    }

  /// Welch's t-test for a given Welch statistic using a 0.01% confidence level.
  let welchTest w =
    if Double.IsNaN w.T then None
    elif abs w.T > Array.get tInv01 (min w.DF (Array.length tInv01) - 1) then sign w.T |> Some
    elif abs w.T < Array.get tInv99 (min w.DF (Array.length tInv99) - 1) then Some 0
    else None

  type RankCount = {mutable Count1:int; mutable Count2:int}
  let mannWhitneyZScore (s:SortedList<_,RankCount>) =
    let mutable n1 = 0
    let mutable n2 = 0
    let mutable r1 = 0
    let mutable r2 = 0
    let mutable st = 0
    s.Values
    |> Seq.iter (fun v ->
      let c1 = v.Count1
      let c2 = v.Count2
      let c = c1 + c2
      let r = (n1+n2) * 2 + c + 1
      n1 <- n1 + c1
      n2 <- n2 + c2
      r1 <- r1 + r * c1
      r2 <- r2 + r * c2
      st <- st + c*c*c - c
    )
    let u1 = r1 - n1*(n1+1)
    let u2 = r2 - n2*(n2+1)
    let u = float(min u1 u2)
    let m = float(n1 * n2)
    let n = n1 + n2
    let s = sqrt(m / 3.0 * (float(n+1) - float st / float(n*(n-1))))
    if u1>u2 then (m-u)/s else (u-m)/s