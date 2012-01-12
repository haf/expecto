namespace Fuchu

open System
open System.Linq
open System.Runtime.CompilerServices

type TestCode = unit -> unit

type Test = 
    | TestCase of TestCode
    | TestList of Test list
    | TestLabel of string * Test


[<AutoOpen>]
[<Extension>]
module F =
    type TimeSpan with
        static member sum = Seq.fold (+) TimeSpan.Zero

    let withLabel label test = TestLabel (label, test)

    type TestResult = 
        | Passed
        | Failed of string
        | Exception of exn

    let testResultToString =
        function
        | Passed -> "Passed"
        | Failed error -> "Failed: " + error
        | Exception e -> "Exception: " + e.ToString()

    type TestResultCounts = {
        Passed: int
        Failed: int
        Errored: int
        Time: TimeSpan
    }
        with 
        override x.ToString() =
                        sprintf "%d tests run: %d passed, %d failed, %d errored (%A)\n"
                            (x.Errored + x.Failed + x.Passed)
                            x.Passed
                            x.Failed
                            x.Errored
                            x.Time
        static member (+) (c1: TestResultCounts, c2: TestResultCounts) = 
            { Passed = c1.Passed + c2.Passed
              Failed = c1.Failed + c2.Failed
              Errored = c1.Errored + c2.Errored
              Time = c1.Time + c2.Time }
            

    let testResultCountsToErrorLevel (c: TestResultCounts) =
        (if c.Failed > 0 then 1 else 0) ||| (if c.Errored > 0 then 2 else 0)

    type TestRunResult = {
        Name: string
        Result: TestResult
        Time: TimeSpan
    }

    let sumTestResults (results: #seq<TestRunResult>) =
        let counts = 
            results 
            |> Seq.map (fun r -> r.Result)
            |> Seq.countBy (function
                            | Passed -> 0
                            | Failed _ -> 1
                            | Exception _ -> 2)
            |> dict
        let get i = 
            match counts.TryGetValue i with
            | true, v -> v
            | _ -> 0

        { Passed = get 0
          Failed = get 1
          Errored = get 2 
          Time = results |> Seq.map (fun r -> r.Time) |> TimeSpan.sum }

    let toTestCodeList =
        let rec loop parentName testList =
            function
            | TestLabel (name, test) -> 
                let fullName = 
                    if parentName = null
                        then name
                        else parentName + "/" + name
                loop fullName testList test
            | TestCase test -> (parentName, test)::testList
            | TestList tests -> List.collect (loop parentName testList) tests
        loop null []

    let evalTestList =
        let failExceptions = [ 
            "NUnit.Framework.AssertionException"
            "Gallio.Framework.Assertions.AssertionFailureException"
            "Xunit.Sdk.AssertException"
        ]
        fun beforeRun onPassed onFailed onException map ->
            let execOne (name: string, test) = 
                beforeRun name
                let w = System.Diagnostics.Stopwatch.StartNew()
                try                    
                    test()
                    w.Stop()
                    onPassed name w.Elapsed
                    { Name = name
                      Result = Passed
                      Time = w.Elapsed }
                with e ->
                    w.Stop()
                    if List.exists ((=) (e.GetType().FullName)) failExceptions
                        then 
                            onFailed name e.Message w.Elapsed
                            { Name = name
                              Result = Failed e.Message
                              Time = w.Elapsed }
                        else
                            onException name e w.Elapsed
                            { Name = name
                              Result = Failed e.Message
                              Time = w.Elapsed }
            map execOne

    let eval beforeRun onPassed onFailed onException map tests =
        let r = toTestCodeList tests |> evalTestList beforeRun onPassed onFailed onException map
        Seq.toList r

    let printPassed = printfn "%s: Passed (%A)"
    let printFailed = printfn "%s: Failed: %s (%A)"
    let printException = printfn "%s: Exception: %A (%A)"

    let evalSeq = eval ignore printPassed printFailed printException Seq.map

    let pmap (f: _ -> _) (s: _ seq) = s.AsParallel().Select f

    let evalPar =
        let locker = obj()
        let printPassed name time = 
            lock locker (fun () -> printPassed name time)
        let printFailed name error time =
            lock locker (fun () -> printFailed name error time)
        let printException name ex time =
            lock locker (fun () -> printException name ex time)
        eval ignore printPassed printFailed printException pmap

    let runEval eval tests = 
        let results = eval tests
        let summary = sumTestResults results
        Console.WriteLine summary
        testResultCountsToErrorLevel summary

    [<Extension>]
    [<CompiledName("Run")>]
    let run tests = runEval evalSeq tests

    [<Extension>]
    [<CompiledName("RunParallel")>]
    let runParallel tests = runEval evalPar tests

open System.Reflection

[<Extension>]
type Test with
    static member NewCase (f: Action) = 
        TestCase f.Invoke

    static member NewCase (label, f: Action) = 
        TestCase f.Invoke |> withLabel label

    static member NewList ([<ParamArray>] tests) = 
        Array.toList tests |> TestList

    static member NewList ([<ParamArray>] tests) =
        tests |> Array.map Test.NewCase |> Test.NewList

    [<Extension>]
    static member WithLabel (test, label) = TestLabel (label, test)

    [<Extension>]
    static member Add (test, add) = TestList [test; TestCase add]

    [<Extension>]
    static member Add (test, add) = TestList [test; add]

    static member FromMember (m: MemberInfo) =
        let toFunc (m: MethodInfo) = Action(fun () -> unbox (m.Invoke(null, [||])))
        [m]
        |> Seq.filter (fun m -> m.MemberType = MemberTypes.Method)
        |> Seq.map (fun m -> m :?> MethodInfo)
        |> Seq.filter (fun m -> m.ReturnType = typeof<System.Void> && m.GetParameters().Length = 0)
        |> Seq.map (fun m -> m.Name, toFunc m)
        |> Seq.map (fun (name, code) -> Test.NewCase code |> withLabel name)
        |> Seq.toList
        |> TestList

    static member FromType (t: Type) =
        t.GetMethods(BindingFlags.Public ||| BindingFlags.Static)
        |> Seq.map Test.FromMember
        |> Seq.toList
        |> TestList
        |> withLabel t.Name

    static member FromAssembly (a: Assembly) =
        a.GetExportedTypes()
        |> Seq.map Test.FromType
        |> Seq.toList
        |> TestList
        |> withLabel (a.FullName.Split ',').[0]

        