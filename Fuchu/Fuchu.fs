namespace Fuchu

open System
open System.Linq
open System.Runtime.CompilerServices
open System.Reflection

type TestCode = unit -> unit

type Test = 
    | TestCase of TestCode
    | TestList of Test list
    | TestLabel of string * Test
    with
        static member toTestCodeList =
            let rec loop parentName testList =
                function
                | TestLabel (name, test) -> 
                    let fullName = 
                        if String.IsNullOrEmpty parentName
                            then name
                            else parentName + "/" + name
                    loop fullName testList test
                | TestCase test -> (parentName, test)::testList
                | TestList tests -> List.collect (loop parentName testList) tests
            loop null []

        static member wrap f =
            let rec loop = 
                function
                | TestCase test -> TestCase (f test)
                | TestList testList -> TestList (List.map loop testList)
                | TestLabel (label, test) -> TestLabel (label, loop test)
            loop

        static member filter pred =
            Test.toTestCodeList 
            >> Seq.filter (fst >> pred)
            >> Seq.map (fun (name, test) -> TestLabel (name, TestCase test))
            >> Seq.toList
            >> TestList

[<AutoOpen>]
[<Extension>]
module F =
    let inline internal (==) x y = LanguagePrimitives.PhysicalEquality x y

    type internal TimeSpan with
        static member sum = Seq.fold (+) TimeSpan.Zero

    let withLabel label test = TestLabel (label, test)

    let inline (->>) label testList =
        TestList testList |> withLabel label

    let inline (-->) label t = 
        TestCase t |> withLabel label

    let bracket setup teardown f () =
        let v = setup()
        try
            f v
            teardown v
        with e ->
            teardown v
            reraise()

    type TestResult = 
        | Passed
        | Ignored of string
        | Failed of string
        | Exception of exn
        override x.ToString() = 
            match x with
            | Passed -> "Passed"
            | Ignored reason -> "Ignored: " + reason
            | Failed error -> "Failed: " + error
            | Exception e -> "Exception: " + e.ToString()
        static member isPassed =
            function
            | Passed -> true
            | _ -> false
        static member isIgnored =
            function
            | Ignored _ -> true
            | _ -> false
        static member isFailed =
            function
            | Failed _ -> true
            | _ -> false
        static member isException =
            function
            | Exception _ -> true
            | _ -> false


    type TestResultCounts = {
        Passed: int
        Ignored: int
        Failed: int
        Errored: int
        Time: TimeSpan
    }
        with 
        override x.ToString() =
                        sprintf "%d tests run: %d passed, %d ignored, %d failed, %d errored (%A)\n"
                            (x.Errored + x.Failed + x.Passed)
                            x.Passed
                            x.Ignored
                            x.Failed
                            x.Errored
                            x.Time
        static member (+) (c1: TestResultCounts, c2: TestResultCounts) = 
            { Passed = c1.Passed + c2.Passed
              Ignored = c1.Ignored + c2.Ignored
              Failed = c1.Failed + c2.Failed
              Errored = c1.Errored + c2.Errored
              Time = c1.Time + c2.Time }
        static member toErrorLevel (c: TestResultCounts) =
            (if c.Failed > 0 then 1 else 0) ||| (if c.Errored > 0 then 2 else 0)
        member x.ToErrorLevel() = TestResultCounts.toErrorLevel x

    [<StructuredFormatDisplay("{Description}")>]
    type TestRunResult = {
        Name: string
        Result: TestResult
        Time: TimeSpan
    }        
        with 
        override x.ToString() = 
            sprintf "%s: %s (%A)" x.Name (x.Result.ToString()) x.Time
        member x.Description = x.ToString()
        static member isPassed (r: TestRunResult) = TestResult.isPassed r.Result
        static member isIgnored (r: TestRunResult) = TestResult.isIgnored r.Result
        static member isFailed (r: TestRunResult) = TestResult.isFailed r.Result
        static member isException (r: TestRunResult) = TestResult.isException r.Result
        static member isFailedOrException r = TestRunResult.isFailed r || TestRunResult.isException r

    let sumTestResults (results: #seq<TestRunResult>) =
        let counts = 
            results 
            |> Seq.map (fun r -> r.Result)
            |> Seq.countBy (function
                            | Passed -> 0
                            | Ignored _ -> 1
                            | Failed _ -> 2
                            | Exception _ -> 3)
            |> dict
        let get i = 
            match counts.TryGetValue i with
            | true, v -> v
            | _ -> 0

        { Passed = get 0
          Ignored = get 1
          Failed = get 2
          Errored = get 3
          Time = results |> Seq.map (fun r -> r.Time) |> TimeSpan.sum }

    let evalTestList =
        let failExceptions = [ 
            "NUnit.Framework.AssertionException"
            "Gallio.Framework.Assertions.AssertionFailureException"
            "Xunit.Sdk.AssertException"
        ]
        let ignoreExceptions = [
            "NUnit.Framework.IgnoreException"
        ]
        let (|ExceptionInList|_|) l e = 
            if List.exists ((=) (e.GetType().FullName)) l
                then Some()
                else None
        fun beforeRun onPassed onIgnored onFailed onException map ->
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
                    match e with
                    | ExceptionInList failExceptions ->
                        onFailed name e.Message w.Elapsed
                        { Name = name
                          Result = Failed e.Message
                          Time = w.Elapsed }
                    | ExceptionInList ignoreExceptions ->
                        onIgnored name e.Message
                        { Name = name
                          Result = Ignored e.Message
                          Time = w.Elapsed }
                    | _ ->
                        onException name e w.Elapsed
                        { Name = name
                          Result = Failed e.Message
                          Time = w.Elapsed }
            map execOne

    let eval beforeRun onPassed onIgnored onFailed onException map tests =
        let r = Test.toTestCodeList tests |> evalTestList beforeRun onPassed onIgnored onFailed onException map
        Seq.toList r

    let printPassed = printfn "%s: Passed (%A)"
    let printIgnored = printfn "%s: Ignored: %s"
    let printFailed = printfn "%s: Failed: %s (%A)"
    let printException = printfn "%s: Exception: %A (%A)"

    let evalSeq = eval ignore printPassed printIgnored printFailed printException Seq.map

    let pmap (f: _ -> _) (s: _ seq) = s.AsParallel().Select f

    let evalPar =
        let locker = obj()
        let printPassed name time = 
            lock locker (fun () -> printPassed name time)
        let printIgnored name reason = 
            lock locker (fun () -> printIgnored name reason)
        let printFailed name error time =
            lock locker (fun () -> printFailed name error time)
        let printException name ex time =
            lock locker (fun () -> printException name ex time)
        eval ignore printPassed printIgnored printFailed printException pmap

    let evalSilent = 
        let ignore2 _ _ = ()
        let ignore3 _ _ _ = ()
        eval ignore ignore2 ignore2 ignore3 ignore3 Seq.map

    let runEval eval tests = 
        let results = eval tests
        let summary = sumTestResults results
        Console.WriteLine summary
        summary.ToErrorLevel()

    [<Extension>]
    [<CompiledName("Run")>]
    let run tests = runEval evalSeq tests
    
    [<Extension>]
    [<CompiledName("RunParallel")>]
    let runParallel tests = runEval evalPar tests

    type internal Type with
        member t.HasAttribute (attr: string) =
            t.GetCustomAttributes true
            |> Seq.filter (fun a -> a.GetType().FullName = attr)
            |> Seq.length |> (<) 0

    type internal MemberInfo with
        member m.HasAttribute (attr: string) =
            m.GetCustomAttributes true
            |> Seq.filter (fun a -> a.GetType().FullName = attr)
            |> Seq.length |> (<) 0


[<Extension>]
type Test with
    [<Extension>]
    static member Switch(test, testCase: Func<_,_>, testList: Func<_,_>, testLabel: Func<_,_,_>) =
        match test with
        | TestCase c -> testCase.Invoke c
        | TestList l -> testList.Invoke l
        | TestLabel (label, t) -> testLabel.Invoke(label,t)

    static member Case (f: Action) = 
        TestCase f.Invoke

    static member Case (label, f: Action) = 
        TestCase f.Invoke |> withLabel label

    static member List ([<ParamArray>] tests) = 
        Array.toList tests |> TestList

    static member List (name, [<ParamArray>] tests) = 
        Array.toList tests |> TestList |> withLabel name

    static member List (name, tests: Func<Test seq>) = 
        Test.List(name, tests.Invoke() |> Seq.toArray)

    static member List ([<ParamArray>] tests) =
        tests |> Array.map Test.Case |> Test.List

    [<Extension>]
    static member WithLabel (test, label) = TestLabel (label, test)

    static member FromMember (m: MemberInfo) =
        let toFunc (m: MethodInfo) = Action(fun () -> unbox (m.Invoke(null, [||])))
        [m]
        |> Seq.filter (fun m -> m.MemberType = MemberTypes.Method)
        |> Seq.map (fun m -> m :?> MethodInfo)
        |> Seq.filter (fun m -> m.ReturnType = typeof<System.Void> && m.GetParameters().Length = 0)
        |> Seq.map (fun m -> m.Name, toFunc m)
        |> Seq.map (fun (name, code) -> Test.Case code |> withLabel name)
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


    static member FromNUnitType (t: Type) =
        let testType = 
            [t]
            |> Seq.filter (fun t -> t.HasAttribute "NUnit.Framework.TestFixtureAttribute")
        let methods = 
            testType
            |> Seq.collect (fun _ -> t.GetMethods())
            |> Seq.toList
        let inline methodsWithAttr attr = 
            methods
            |> Seq.filter (fun m -> m.HasAttribute attr)
            |> Seq.toList
        let testMethods = methodsWithAttr "NUnit.Framework.TestAttribute"
        let setupMethods = methodsWithAttr "NUnit.Framework.SetUpAttribute"
        let teardownMethods = methodsWithAttr "NUnit.Framework.TearDownAttribute"
        let fixtureSetupMethods = methodsWithAttr "NUnit.Framework.TestFixtureSetUpAttribute"

        let inline invoke o (m: MethodInfo) =
            m.Invoke(o, null) |> ignore

        TestList [
            for t in testType ->
                t.FullName ->> [
                    for m in testMethods ->
                        let o = Activator.CreateInstance t
                        let inline invoke x = invoke o x
                        Seq.iter invoke fixtureSetupMethods
                        m.Name --> 
                            fun () -> 
                                try
                                    Seq.iter invoke setupMethods
                                    invoke m
                                finally
                                    Seq.iter invoke teardownMethods
                ]
        ]

    static member Setup (setup: Func<_>, teardown: Action<_>) =
        if setup == null then raise (ArgumentNullException("setup"))
        if teardown == null then raise (ArgumentNullException("teardown"))
        let f (test: Action<_>) = 
            if test == null then raise (ArgumentNullException("test"))
            let r = bracket setup.Invoke teardown.Invoke test.Invoke
            Action r
        Func<_,_> f

    static member Setup (setup: Func<'a>): Func<Action<'a>, Action> when 'a :> IDisposable =
        Test.Setup(setup, fun d -> d.Dispose())

    [<Extension>]
    static member Wrap (test, f: Func<_,_>) = Test.wrap f.Invoke test