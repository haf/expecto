namespace Fuchu

open System
open System.Linq
open System.Runtime.CompilerServices
open System.Reflection

/// Actual test function
type TestCode = unit -> unit

/// Test tree
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
    let ignore2 _ _ = ()
    let ignore3 _ _ _ = ()

    let tryGetType t = 
        try
            Type.GetType(t, true) |> Some
        with _ -> None

    let disposable f = 
        { new IDisposable with
            member x.Dispose() = f() }

    let bracket setup teardown f () =
        let v = setup()
        use dummy = disposable (fun () -> teardown v)
        f v

    let tprintf fmt = 
        Printf.kprintf (fun s -> 
                            System.Diagnostics.Trace.Write s
                            Console.Write s) fmt

    open System.Text.RegularExpressions
    let rx = lazy Regex(" at (.*) in (.*):line (\d+)", RegexOptions.Compiled ||| RegexOptions.Multiline)
    let stackTraceToString s = rx.Value.Replace(s, "$2($3,1): $1")
    let exnToString (e: Exception) = stackTraceToString (e.ToString())

    module Seq =
        let cons x xs = seq { yield x; yield! xs }

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
    open Helpers

    /// Flattens a tree of tests
    let toTestCodeList =
        let rec loop parentName testList =
            function
            | TestLabel (name, test) -> 
                let fullName = 
                    if String.IsNullOrEmpty parentName
                        then name
                        else parentName + "/" + name
                loop fullName testList test
            | TestCase test -> Seq.cons (parentName, test) testList
            | TestList tests -> Seq.collect (loop parentName testList) tests
        loop null Seq.empty

    /// Maps all TestCodes in a Test
    let rec wrap f =
        function
        | TestCase test -> TestCase (f test)
        | TestList testList -> TestList (Seq.map (wrap f) testList)
        | TestLabel (label, test) -> TestLabel (label, wrap f test)

    /// Filter tests by name
    let filter pred =
        toTestCodeList 
        >> Seq.filter (fst >> pred)
        >> Seq.map (fun (name, test) -> TestLabel (name, TestCase test))
        >> TestList

    /// Applies a timeout to a test
    let timeout timeout (test: TestCode) : TestCode =
        let testFunc = Func<_,_> test
        let asyncTestFunc = Async.FromBeginEnd((fun (b,c) -> testFunc.BeginInvoke((),b,c)), testFunc.EndInvoke)
        fun () -> 
            try
                Async.RunSynchronously(asyncTestFunc, timeout = timeout)
            with :? TimeoutException ->
                let ts = TimeSpan.FromMilliseconds (float timeout)
                raise <| AssertException(sprintf "Timeout (%A)" ts)


module Impl =
    open Helpers

    type TestResult = 
        | Passed
        | Ignored of string
        | Failed of string
        | Error of exn
        override x.ToString() = 
            match x with
            | Passed -> "Passed"
            | Ignored reason -> "Ignored: " + reason
            | Failed error -> "Failed: " + error
            | Error e -> "Exception: " + exnToString e
        static member tag = 
            function
            | Passed -> 0
            | Ignored _ -> 1
            | Failed _ -> 2
            | Error _ -> 3
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
            | Error _ -> true
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
        static member errorCode (c: TestResultCounts) =
            (if c.Failed > 0 then 1 else 0) ||| (if c.Errored > 0 then 2 else 0)

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
            |> Seq.countBy TestResult.tag
            |> dict
        let get result = 
            match counts.TryGetValue (TestResult.tag result) with
            | true, v -> v
            | _ -> 0

        { Passed = get TestResult.Passed
          Ignored = get (TestResult.Ignored "")
          Failed = get (TestResult.Failed "")
          Errored = get (TestResult.Error null)
          Time = results |> Seq.map (fun r -> r.Time) |> Seq.fold (+) TimeSpan.Zero }

    type TestPrinters = {
        BeforeRun: string -> unit
        Passed: string -> TimeSpan -> unit
        Ignored: string -> string -> unit
        Failed: string -> string -> TimeSpan -> unit
        Exception: string -> exn -> TimeSpan -> unit
    } with
        static member Default = {
            BeforeRun = ignore
            Passed = ignore2
            Ignored = ignore2
            Failed = ignore3
            Exception = ignore3
        }


    let evalTestList =
        let failExceptions = [
            typeof<AssertException>.AssemblyQualifiedName
            "NUnit.Framework.AssertionException, NUnit.Framework"
            "Gallio.Framework.Assertions.AssertionFailureException, Gallio"
            "Gallio.Framework.Assertions.AssertionException, Gallio"
            "Xunit.Sdk.AssertException, Xunit"
        ]
        let ignoreExceptions = [
            "NUnit.Framework.IgnoreException, NUnit.Framework"
        ]
        let failExceptionTypes = lazy List.choose tryGetType failExceptions
        let ignoreExceptionTypes = lazy List.choose tryGetType ignoreExceptions

        let (|ExceptionInList|_|) (l: Type list) (e: #exn) = 
            let et = e.GetType()
            if l |> List.exists (fun x -> x.IsAssignableFrom et)
                then Some()
                else None

        fun (printers: TestPrinters) map ->
            let execOne (name: string, test) = 
                printers.BeforeRun name
                let w = System.Diagnostics.Stopwatch.StartNew()
                try
                    test()
                    w.Stop()
                    printers.Passed name w.Elapsed
                    { Name = name
                      Result = Passed
                      Time = w.Elapsed }
                with e ->
                    w.Stop()
                    match e with
                    | ExceptionInList failExceptionTypes.Value ->
                        let msg =
                            let firstLine = 
                                (stackTraceToString e.StackTrace).Split('\n') 
                                |> Seq.filter (fun q -> q.Contains ",1): ") 
                                |> Enumerable.FirstOrDefault
                            sprintf "\n%s\n%s\n" e.Message firstLine
                        printers.Failed name msg w.Elapsed
                        { Name = name
                          Result = Failed msg
                          Time = w.Elapsed }
                    | ExceptionInList ignoreExceptionTypes.Value ->
                        printers.Ignored name e.Message
                        { Name = name
                          Result = Ignored e.Message
                          Time = w.Elapsed }
                    | _ ->
                        printers.Exception name e w.Elapsed
                        { Name = name
                          Result = TestResult.Error e
                          Time = w.Elapsed }
            map execOne

    let eval (printer: TestPrinters) map tests =
        Test.toTestCodeList tests 
        |> evalTestList printer map
        |> Seq.toList

    let printFailed = tprintf "%s: Failed: %s (%A)\n"
    let printException name ex = tprintf "%s: Exception: %s (%A)\n" name (exnToString ex)

    let evalSeq = 
        let printer = 
            { TestPrinters.Default with 
                Failed = printFailed
                Exception = printException }

        eval printer Seq.map

    let pmap (f: _ -> _) (s: _ seq) = s.AsParallel().Select(f) :> _ seq

    let evalPar =
        let flock =
            let locker = obj()
            lock locker
        let inline flock3 f a b c = flock (fun () -> f a b c)
        let printFailed = flock3 printFailed 
        let printException = flock3 printException
        let printer = 
            { TestPrinters.Default with 
                Failed = printFailed
                Exception = printException }
        eval printer pmap

    let runEval eval (tests: Test) = 
        let w = System.Diagnostics.Stopwatch.StartNew()
        let results = eval tests
        w.Stop()
        let summary = { sumTestResults results with Time = w.Elapsed }
        tprintf "%s" (summary.ToString())
        TestResultCounts.errorCode summary

    let testFromMember (m: MemberInfo): Test option =
        [m]
        |> List.filter (fun m -> m.HasAttributeType typeof<TestsAttribute>)
        |> List.choose (fun m ->
                            match box m with
                            | :? FieldInfo as m ->
                                if m.FieldType = typeof<Test>
                                    then Some(unbox (m.GetValue(null)))
                                    else None
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

    let listToTestListOption = 
        function
        | [] -> None
        | x -> Some (TestList x)
        
    let testFromType =
        let asMembers x = Seq.map (fun m -> m :> MemberInfo) x
        let bindingFlags = BindingFlags.Public ||| BindingFlags.Static
        fun (t: Type) ->
            [ t.GetMethods bindingFlags |> asMembers
              t.GetProperties bindingFlags |> asMembers
              t.GetFields bindingFlags |> asMembers ]
            |> Seq.collect id
            |> Seq.choose testFromMember
            |> Seq.toList
            |> listToTestListOption

    let testFromAssemblyWithFilter typeFilter (a: Assembly) =
        a.GetExportedTypes()
        |> Seq.filter typeFilter
        |> Seq.choose testFromType
        |> Seq.toList
        |> listToTestListOption

    /// Gets tests marked with TestsAttribute from an assembly
    let testFromAssembly = testFromAssemblyWithFilter (fun _ -> true)

    let testFromThisAssembly () = testFromAssembly (Assembly.GetEntryAssembly())

[<AutoOpen; Extension>]
module Tests =
    open Impl
    open Helpers

    let inline failtest msg = raise <| AssertException msg
    let inline failtestf fmt = Printf.ksprintf (fun msg -> raise <| AssertException msg) fmt

    let inline testList name tests = TestLabel(name, TestList tests)
    let inline testCase name test = TestLabel(name, TestCase test)
    let inline testFixture setup = 
         Seq.map (fun (name, partialTest) ->
                        testCase name (setup partialTest))

    let inline (=>>) name tests = testList name tests
    let inline (=>) name test = testCase name test
    let inline (+>) f = testFixture f
    let inline (==>) name test = name,test

    /// Runs tests
    [<Extension; CompiledName("Run")>]
    let run tests = runEval evalSeq tests
    
    /// Runs tests in parallel
    [<Extension; CompiledName("RunParallel")>]
    let runParallel tests = runEval evalPar tests

    type RunOptions = { Parallel: bool }
    let parseArgs =
        let defaultOptions = { RunOptions.Parallel = false }
        let opts = [ "/m", fun o -> { o with RunOptions.Parallel = true } ]
        fun (args: string[]) ->
            (defaultOptions, args) 
            ||> Seq.fold (fun opt arg -> 
                            (opt, opts) ||> Seq.fold (fun o (a,f) -> if a = arg then f o else o))

    /// Runs tests with supplied options. Returns 0 if all tests passed, otherwise 1
    [<CompiledNameAttribute("DefaultMainWithOptions")>]
    let defaultMainWithOptions tests (options: RunOptions) = 
        let run = if options.Parallel then runParallel else run
        run tests
    
    /// Runs tests with supplied command-line options. Returns 0 if all tests passed, otherwise 1
    [<CompiledNameAttribute("DefaultMain")>]
    let defaultMain tests = parseArgs >> defaultMainWithOptions tests

    /// Runs tests in this assembly with supplied command-line options. Returns 0 if all tests passed, otherwise 1
    [<CompiledNameAttribute("DefaultMainThisAssembly")>]
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
        label => f.Invoke

    static member Case (label: string, f: Action<_>) = 
        label ==> f

    [<Extension>]
    static member List (tests, name) = 
        name =>> tests

    [<Extension>]
    static member List tests = 
        TestList tests

    static member List (name, [<ParamArray>] tests: Test[]) = 
        name =>> tests

    static member List ([<ParamArray>] tests) =
        tests |> Seq.map Test.Case |> TestList

    static member List (name, setup: Func<_,_>, [<ParamArray>] tests) =
        let tests = tests |> Array.map (fun (name, test) -> Test.Case(name, setup.Invoke test))
        Test.List(name, tests)

    [<Extension>]
    static member Run tests = TestList tests |> run

    static member Fixture (setup: Func<_>, teardown: Action<_>) =
        if setup = null then raise (ArgumentNullException("setup"))
        if teardown = null then raise (ArgumentNullException("teardown"))
        let f (test: Action<_>) = 
            if test = null then raise (ArgumentNullException("test"))
            let r = Helpers.bracket setup.Invoke teardown.Invoke test.Invoke
            Action r
        Func<_,_> f

    /// Maps all TestCodes in a Test
    [<Extension>]
    static member Wrap (test, f: Func<Action,Action>) = 
        test |> Test.wrap (fun t -> f.Invoke(Action t).Invoke)

    /// Applies a timeout to a test
    [<Extension>]
    static member Timeout(test: Action, timeout) = 
        Action(Test.timeout timeout test.Invoke)

    /// Filter tests by name
    [<Extension>]
    static member Where(test, pred: Func<_,_>) = 
        Test.filter pred.Invoke test