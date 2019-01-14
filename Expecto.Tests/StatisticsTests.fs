module Expecto.StatisticsTests

open Expecto
open System.Collections.Generic

[<Tests>]
let statisticsTests =
  ftestList "statistics tests" [

    test "sorted list" {
      let l = SortedList()
      l.Add 10L
      l.Add 7L
      l.Add 11L
      l.Add 8L
      l.Add 9L
      Expect.sequenceEqual (l.ToSeq()) [7L;8L;9L;10L;11L] "sorted"
    }

    test "min rank sum" {
      let s = SortedList<int,Statistics.RankCount>()
      [|
        3, 9
        4, 7
        2, 5
        6, 10
        2, 6
        5, 8
      |]
      |> Array.iter (fun (k1,k2) ->
        match s.TryGetValue k1 with
        | false,_ -> s.Add(k1, {Count1=1;Count2=0})
        | true,r -> r.Count1 <- r.Count1 + 1
        match s.TryGetValue k2 with
        | false,_ -> s.Add(k2, {Count1=0;Count2=1})
        | true,r -> r.Count2 <- r.Count2 + 1
      )
      let u,_,_,_ = Statistics.mannWhitneyUTest s
      Expect.equal u 2.0 "u=2.0"
    }

    test "z score" {
      let s = SortedList<int,Statistics.RankCount>()
      [|
        26, 18
        21, 23
        22, 21
        26, 20
        19, 20
        22, 29
        26, 20
        25, 16
        24, 20
        21, 26
        23, 21
        23, 25
        18, 17
        29, 18
        22, 19
      |]
      |> Array.iter (fun (k1,k2) ->
        match s.TryGetValue k1 with
        | false,_ -> s.Add(k1, {Count1=1;Count2=0})
        | true,r -> r.Count1 <- r.Count1 + 1
        match s.TryGetValue k2 with
        | false,_ -> s.Add(k2, {Count1=0;Count2=1})
        | true,r -> r.Count2 <- r.Count2 + 1
      )
      let u,m,s,z = Statistics.mannWhitneyUTest s
      Expect.equal u 62.0 "u=62.0"
      Expect.equal m 112.5 "m=112.5"
      Expect.equal s (sqrt(15.0*15.0/12.0*(31.0-270.0/30.0/29.0))) "s=23.988143623"
      Expect.equal z ((m-u)/s) "z=2.105207"
    }
  ]