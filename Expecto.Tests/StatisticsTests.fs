module Expecto.StatisticsTests

open Expecto
open System.Collections.Generic

[<Tests>]
let statisticsTests =

  let rankCount l1 l2 =
    let s = SortedList<int,Statistics.RankCount>()
    Array.iter (fun v ->
        match s.TryGetValue v with
        | false,_ -> s.Add(v, {Count1=1;Count2=0})
        | true,r -> r.Count1 <- r.Count1 + 1
    ) l1
    Array.iter (fun v ->
        match s.TryGetValue v with
        | false,_ -> s.Add(v, {Count1=0;Count2=1})
        | true,r -> r.Count2 <- r.Count2 + 1
    ) l2
    s

  testList "statistics tests" [

    test "sorted list" {
      let l = SortedList()
      l.Add 10L
      l.Add 7L
      l.Add 11L
      l.Add 8L
      l.Add 9L
      Expect.sequenceEqual (l.ToSeq()) [7L;8L;9L;10L;11L] "sorted"
    }

    test "z score video example u=2" { // https://www.youtube.com/watch?v=BT1FKd1Qzjw
      let s = rankCount [|3;4;2; 6;2;5|]
                        [|9;7;5;10;6;8|]
      let z = Statistics.mannWhitneyZScore s
      Expect.floatClose Accuracy.high z -2.5755944343440005 "z"
    }

    test "z score" { // https://planetcalc.com/7858/
      let s = rankCount [|26;21;22;26;19;22;26;25;24;21;23;23;18;29;22|]
                        [|18;23;21;20;20;29;20;16;20;26;21;25;17;18;19|]
      let z = Statistics.mannWhitneyZScore s
      Expect.floatClose Accuracy.high z 2.1052066718225415 "z"
    }

    test "z score smaller both" { // https://planetcalc.com/7858/
      let s = rankCount [|26;21;22;26;19;22;26;25;24;21;23;23|]
                        [|18;23;21;20;20;29;20;16;20;26;21;25|]
      let z = Statistics.mannWhitneyZScore s
      Expect.floatClose Accuracy.high z 1.5711904963981076 "z"
    }

    test "z score smaller 1" { // https://planetcalc.com/7858/
      let s = rankCount [|26;21;22;26;19;22;26;25;24;21;23;23|]
                        [|18;23;21;20;20;29;20;16;20;26;21;25;17;18;19|]
      let z = Statistics.mannWhitneyZScore s
      Expect.floatClose Accuracy.high z 2.184081750874445 "z"
    }

    test "z score smaller 2" { // https://planetcalc.com/7858/
      let s = rankCount [|26;21;22;26;19;22;26;25;24;21;23;23;18;29;22|]
                        [|18;23;21;20;20;29;20;16;20;26;21;25|]
      let z = Statistics.mannWhitneyZScore s
      Expect.floatClose Accuracy.high z 1.4730972323477298 "z"
    }
  ]