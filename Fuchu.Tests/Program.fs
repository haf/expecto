open System
open Fuchu
open NUnit.Framework

let tests = 
    "" ->> [
        "basic" --> 
            fun () -> Assert.AreEqual(4, 2+2)
        "sumTestResults" ->> [
            let sumTestResultsTests = 
                let r1 = { TestRunResult.Name = ""; Result = Passed; Time = TimeSpan.FromMinutes 2. }
                let r2 = { TestRunResult.Name = ""; Result = Exception (ArgumentException()); Time = TimeSpan.FromMinutes 3. }
                let r3 = { TestRunResult.Name = ""; Result = Failed ""; Time = TimeSpan.FromMinutes 4. }
                let r4 = { TestRunResult.Name = ""; Result = Passed; Time = TimeSpan.FromMinutes 5. }
                let r5 = { TestRunResult.Name = ""; Result = Failed ""; Time = TimeSpan.FromMinutes 6. }
                let r6 = { TestRunResult.Name = ""; Result = Passed; Time = TimeSpan.FromMinutes 7. }
                [r1;r2;r3;r4;r5;r6]
            let r = sumTestResults sumTestResultsTests
            yield "Passed" -->
                fun () -> Assert.AreEqual(3, r.Passed)
            yield "Failed" -->
                fun () -> Assert.AreEqual(2, r.Failed)
            yield "Exception" -->
                fun () -> Assert.AreEqual(1, r.Errored)
            yield "Time" -->
                fun () -> Assert.AreEqual(TimeSpan.FromMinutes 27., r.Time)
        ]
        "TestResultCounts" ->> [
            let c1 = { Passed = 1; Failed = 2; Errored = 3; Time = TimeSpan.FromSeconds 20. }
            yield "plus" ->> [
                let c2 = { Passed = 2; Failed = 3; Errored = 4; Time = TimeSpan.FromSeconds 25. }
                let r = c1 + c2
                yield "Passed" --> fun () -> Assert.AreEqual(3, r.Passed)
                yield "Failed" --> fun () -> Assert.AreEqual(5, r.Failed)
                yield "Errored" --> fun () -> Assert.AreEqual(7, r.Errored)
                yield "Time" --> fun () -> Assert.AreEqual(TimeSpan.FromSeconds 45., r.Time)
            ]
            yield "ToString" --> 
                fun () -> Assert.AreEqual("6 tests run: 1 passed, 2 failed, 3 errored (00:00:20)\n", c1.ToString())
        ]
    ]

[<EntryPoint>]
let main args =
    run tests