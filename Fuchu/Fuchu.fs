namespace Fuchu

open System
open System.Linq
open System.Runtime.CompilerServices
open System.Reflection

type TestCode = unit -> unit

type Test = 
    | TestCase of TestCode
    | TestList of Test seq
    | TestLabel of string * Test

type AssertException(msg) =
    inherit Exception(msg)

[<AttributeUsage(AttributeTargets.Method ||| AttributeTargets.Property ||| AttributeTargets.Field)>]
type TestsAttribute() = 
    inherit Attribute()

module Helpers =
    let internal (==) x y = LanguagePrimitives.PhysicalEquality x y

    type TimeSpan with
        static member sum = Seq.fold (+) TimeSpan.Zero

    let ignore2 _ _ = ()
    let ignore3 _ _ _ = ()

    let disposable f = 
        { new IDisposable with
            member x.Dispose() = f() }

    let bracket setup teardown f () =
        let v = setup()
        use dummy = disposable (fun () -> teardown v)
        f v

    open System.Diagnostics
    let traceInit = 
        lazy (
            Trace.AutoFlush <- true
            Trace.Listeners.Add(new ConsoleTraceListener()) |> ignore
        )
    let tprintf fmt = 
        traceInit.Value
        Printf.kprintf Trace.Write fmt

    open System.Text.RegularExpressions
    let rx = lazy Regex(" at (.*) in (.*):line (\d+)", RegexOptions.Compiled ||| RegexOptions.Multiline)
    let stackTraceToString s = rx.Value.Replace(s, "$2($3,1): $1")
    let exnToString (e: Exception) = stackTraceToString (e.ToString())

    type MemberInfo with
        member m.HasAttributePred (pred: Type -> bool) =
            m.GetCustomAttributes true
            |> Seq.filter (fun a -> pred(a.GetType()))
            |> Seq.length |> (<) 0

        member m.HasAttributeType (attr: Type) =
            m.HasAttributePred ((=) attr)

        member m.HasAttribute (attr: string) =
            m.HasAttributePred (fun (t: Type) -> t.FullName = attr)
        
[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module Test =
    let toTestCodeList =
        let rec loop parentName testList =
            function
            | TestLabel (name, test) -> 
                let fullName = 
                    if String.IsNullOrEmpty parentName
                        then name
                        else parentName + "/" + name
                loop fullName testList test
            | TestCase test -> (parentName, test)::(Seq.toList testList) :> _ seq
            | TestList tests -> Seq.collect (loop parentName testList) tests
        loop null Seq.empty

    let rec wrap f =
        function
        | TestCase test -> TestCase (f test)
        | TestList testList -> TestList (Seq.map (wrap f) testList)
        | TestLabel (label, test) -> TestLabel (label, wrap f test)

    let filter pred =
        toTestCodeList 
        >> Seq.filter (fst >> pred)
        >> Seq.map (fun (name, test) -> TestLabel (name, TestCase test))
        >> TestList

    let timeout timeout (test: TestCode) : TestCode =
        let testFunc = Func<_,_> test
        let asyncTestFunc = Async.FromBeginEnd((fun (b,c) -> testFunc.BeginInvoke((),b,c)), testFunc.EndInvoke)
        fun () -> 
            try
                Async.RunSynchronously(asyncTestFunc, timeout = timeout)
            with :? TimeoutException ->
                let ts = TimeSpan.FromMilliseconds (float timeout)
                raise <| AssertException(sprintf "Timeout (%A)" ts)

open Helpers

[<AutoOpen; Extension>]
module Fuchu =

    let withLabel label test = TestLabel (label, test)

    let inline (=>>) label testList =
        TestList testList |> withLabel label

    let inline (=>) label t = 
        TestCase t |> withLabel label

    let inline (+>) f =
         Seq.map (fun (name, partialTest) ->
                        name => f partialTest)

    let inline (==>) name test = name,test

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
            | Exception e -> "Exception: " + exnToString e
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

    [<StructuredFormatDisplay("{Description}")>]
    type TestResultCounts = {
        Passed: int
        Ignored: int
        Failed: int
        Errored: int
        Time: TimeSpan
    } with 
        override x.ToString() =
                        sprintf "%d tests run: %d passed, %d ignored, %d failed, %d errored (%A)\n"
                            (x.Errored + x.Failed + x.Passed)
                            x.Passed x.Ignored x.Failed x.Errored x.Time
        member x.Description = x.ToString()
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
    } with 
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
            "Fuchu.AssertException"
            "NUnit.Framework.AssertionException"
            "Gallio.Framework.Assertions.AssertionFailureException"
            "Gallio.Framework.Assertions.AssertionException"
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
                        let msg =
                            let firstLine = 
                                (stackTraceToString e.StackTrace).Split('\n') 
                                |> Seq.filter (fun q -> q.Contains ",1): ") 
                                |> Enumerable.FirstOrDefault
                            sprintf "%s%s\n" e.Message firstLine
                        onFailed name msg w.Elapsed
                        { Name = name
                          Result = Failed msg
                          Time = w.Elapsed }
                    | ExceptionInList ignoreExceptions ->
                        onIgnored name e.Message
                        { Name = name
                          Result = Ignored e.Message
                          Time = w.Elapsed }
                    | _ ->
                        onException name e w.Elapsed
                        { Name = name
                          Result = TestResult.Exception e
                          Time = w.Elapsed }
            map execOne

    let eval beforeRun onPassed onIgnored onFailed onException map tests =
        Test.toTestCodeList tests 
        |> evalTestList beforeRun onPassed onIgnored onFailed onException map
        |> Seq.toList

    let printFailed = tprintf "%s: Failed: %s (%A)\n"
    let printException name ex = tprintf "%s: Exception: %s (%A)\n" name (exnToString ex)

    let evalSeq = eval ignore ignore2 ignore2 printFailed printException Seq.map

    let pmap (f: _ -> _) (s: _ seq) = s.AsParallel().Select f

    let evalPar =
        let flock =
            let locker = obj()
            lock locker
        let inline flock3 f a b c = flock (fun () -> f a b c)
        let printFailed = flock3 printFailed 
        let printException = flock3 printException
        eval ignore ignore2 ignore2 printFailed printException pmap

    let evalSilent = eval ignore ignore2 ignore2 ignore3 ignore3 Seq.map

    let runEval eval tests = 
        let results = eval tests
        let summary = sumTestResults results
        tprintf "%s" (summary.ToString())
        summary.ToErrorLevel()

    [<Extension; CompiledName("Run")>]
    let run tests = runEval evalSeq tests
    
    [<Extension; CompiledName("RunParallel")>]
    let runParallel tests = runEval evalPar tests

    let testFromMember (m: MemberInfo): Test option =
        [m]
        |> List.filter (fun m -> m.HasAttributeType typeof<TestsAttribute>)
        |> List.choose (fun m ->
                            match box m with
                            | :? MethodInfo as m -> 
                                if m.ReturnType = typeof<Test>
                                    then Some(unbox (m.Invoke(null, null)))
                                    else None
                            | :? PropertyInfo as m -> 
                                if m.PropertyType = typeof<Test>
                                    then Some(unbox (m.GetValue(null, null)))
                                    else None
                            | _ -> None)
        |> List.tryFind (fun _ -> true)

    let testFromType (t: Type) =
        let asMembers x = Seq.map (fun m -> m :> MemberInfo) x
        let bindingFlags = BindingFlags.Public ||| BindingFlags.Static
        let tests = 
            t.GetMethods bindingFlags |> asMembers
            |> Seq.append (t.GetProperties bindingFlags |> asMembers)
            |> Seq.choose testFromMember
            |> Seq.toList
        match tests with
        | [] -> None
        | x -> Some (TestList x)

    let testFromAssemblyWithFilter typeFilter (a: Assembly) =
        let tests = 
            a.GetExportedTypes()
            |> Seq.filter typeFilter
            |> Seq.choose testFromType
            |> Seq.toList
        match tests with
        | [] -> None
        | x -> Some (TestList x)

    let testFromAssembly = testFromAssemblyWithFilter (fun _ -> true)

    type RunOptions = { Parallel: bool }
    let parseArgs =
        let defaultOptions = { RunOptions.Parallel = false }
        let opts = [ "/m", fun o -> { o with RunOptions.Parallel = true } ]
        fun (args: string[]) ->
            (defaultOptions, args) 
            ||> Seq.fold (fun opt arg -> 
                            (opt, opts) ||> Seq.fold (fun o (a,f) -> if a = arg then f o else o))
    let defaultMainWithOptions tests (options: RunOptions) = 
        let run = if options.Parallel then runParallel else run
        run tests
    let defaultMain tests = parseArgs >> defaultMainWithOptions tests
    let defaultMainThisAssembly args = 
        let tests =
            match testFromAssembly (Assembly.GetEntryAssembly()) with
            | Some t -> t
            | None -> TestList []
        defaultMain tests args
        
[<Extension>]
type Test with
    [<Extension>]
    static member Match(test, testCase: Func<_,_>, testList: Func<_,_>, testLabel: Func<_,_,_>) =
        match test with
        | TestCase c -> testCase.Invoke c
        | TestList l -> testList.Invoke l
        | TestLabel (label, t) -> testLabel.Invoke(label,t)

    static member Case (f: Action) = 
        TestCase f.Invoke

    static member Case (label, f: Action) = 
        TestCase f.Invoke |> withLabel label

    static member Case (label: string, f: Action<_>) = 
        label ==> f

    [<Extension>]
    static member List (tests, name) = 
        TestList tests |> withLabel name

    [<Extension>]
    static member List ([<ParamArray>] tests) = 
        TestList tests

    static member List (name, [<ParamArray>] tests) = 
        TestList tests |> withLabel name

    static member List (name, tests: Func<Test seq>) = 
        Test.List(name, tests.Invoke() |> Seq.toArray)

    static member List ([<ParamArray>] tests) =
        tests |> Seq.map Test.Case |> TestList

    static member List (name, setup: Func<_,_>, [<ParamArray>] tests) =
        let tests = tests |> Array.map (fun (name, test) -> Test.Case(name, setup.Invoke test))
        Test.List(name, tests)

    [<Extension>]
    static member WithLabel (test, label) = TestLabel (label, test)

    [<Extension>]
    static member Run tests = TestList tests |> run

    static member Fixture (setup: Func<_>, teardown: Action<_>) =
        if setup == null then raise (ArgumentNullException("setup"))
        if teardown == null then raise (ArgumentNullException("teardown"))
        let f (test: Action<_>) = 
            if test == null then raise (ArgumentNullException("test"))
            let r = Helpers.bracket setup.Invoke teardown.Invoke test.Invoke
            Action r
        Func<_,_> f

    [<Extension>]
    static member Wrap (test, f: Func<Action,Action>) = 
        test |> Test.wrap (fun t -> f.Invoke(Action t).Invoke)

    [<Extension>]
    static member Timeout(test: Action, timeout) = 
        Action(Test.timeout timeout test.Invoke)

    [<Extension>]
    static member Where(test, pred: Func<_,_>) = 
        Test.filter pred.Invoke test