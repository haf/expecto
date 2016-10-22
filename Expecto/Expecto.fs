namespace Expecto
#nowarn "46"

open System
open System.Linq
open System.Runtime.CompilerServices
open System.Reflection

/// Actual test function
type TestCode = unit -> unit

/// Test tree – this is how you compose your tests as values. Since
/// any of these can act as a test, you can pass any of these DU cases
/// into a function that takes a Test.
type Test =
  | TestCase of code:TestCode
  | TestList of tests:Test seq
  | TestLabel of label:string * test:Test

type ExpectoException(msg) = inherit Exception(msg)
type AssertException(msg) = inherit ExpectoException(msg)
type IgnoreException(msg) = inherit ExpectoException(msg)

/// Marks a top-level test for scanning
[<AttributeUsage(AttributeTargets.Method ||| AttributeTargets.Property ||| AttributeTargets.Field)>]
type TestsAttribute() = inherit Attribute()

module Helpers =
  let inline ignore2 _ = ignore
  let inline ignore3 _ = ignore2

  let bracket setup teardown f () =
    let v = setup()
    try
      f v
    finally
      teardown v

  /// Print to Console and Trace
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

  type Type with
    static member TryGetType t =
      try
        Type.GetType(t, true) |> Some
      with _ ->
        None

  type MemberInfo with
    member m.HasAttributePred (pred: Type -> bool) =
      m.GetCustomAttributes true
      |> Seq.filter (fun a -> pred(a.GetType()))
      |> Seq.length |> (<) 0

    member m.HasAttributeType (attr: Type) =
      m.HasAttributePred ((=) attr)

    member m.HasAttribute (attr: string) =
      m.HasAttributePred (fun (t: Type) -> t.FullName = attr)

    member m.GetAttributes (attr: string) : Attribute seq =
      m.GetCustomAttributes true
      |> Seq.filter (fun a -> a.GetType().FullName = attr)
      |> Seq.cast


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

  /// Recursively maps all TestCodes in a Test
  let rec wrap f =
    function
    | TestCase test -> TestCase (f test)
    | TestList testList -> TestList (Seq.map (wrap f) testList)
    | TestLabel (label, test) -> TestLabel (label, wrap f test)

  /// Recursively replaces TestCodes in a Test
  let rec replaceTestCode f =
    function
    | TestLabel (label, TestCase test) -> f label test
    | TestCase test -> f null test
    | TestList testList -> TestList (Seq.map (replaceTestCode f) testList)
    | TestLabel (label, test) -> TestLabel (label, replaceTestCode f test)

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
  type TestResultCounts =
    { passed: int
      ignored: int
      failed: int
      errored: int
      time: TimeSpan }

      override x.ToString() =
        sprintf "%d tests run: %d passed, %d ignored, %d failed, %d errored (%A)\n"
                (x.errored + x.failed + x.passed)
                x.passed x.ignored x.failed x.errored x.time
      member x.Description = x.ToString()
      static member (+) (c1: TestResultCounts, c2: TestResultCounts) =
          { passed = c1.passed + c2.passed
            ignored = c1.ignored + c2.ignored
            failed = c1.failed + c2.failed
            errored = c1.errored + c2.errored
            time = c1.time + c2.time }
      static member errorCode (c: TestResultCounts) =
          (if c.failed > 0 then 1 else 0) ||| (if c.errored > 0 then 2 else 0)

  [<StructuredFormatDisplay("{Description}")>]
  type TestRunResult = {
      name: string
      result: TestResult
      time: TimeSpan
  } with
    override x.ToString() =
     sprintf "%s: %s (%A)" x.name (x.result.ToString()) x.time
    member x.Description = x.ToString()
    static member isPassed (r: TestRunResult) = TestResult.isPassed r.result
    static member isIgnored (r: TestRunResult) = TestResult.isIgnored r.result
    static member isFailed (r: TestRunResult) = TestResult.isFailed r.result
    static member isException (r: TestRunResult) = TestResult.isException r.result
    static member isFailedOrException r = TestRunResult.isFailed r || TestRunResult.isException r

  let sumTestResults (results: #seq<TestRunResult>) =
    let counts =
        results
        |> Seq.map (fun r -> r.result)
        |> Seq.countBy TestResult.tag
        |> dict
    let get result =
        match counts.TryGetValue (TestResult.tag result) with
        | true, v -> v
        | _ -> 0

    { passed = get TestResult.Passed
      ignored = get (TestResult.Ignored "")
      failed = get (TestResult.Failed "")
      errored = get (TestResult.Error null)
      time = results |> Seq.map (fun r -> r.time) |> Seq.fold (+) TimeSpan.Zero }

  /// Hooks to print report through test run
  type TestPrinters =
    { beforeRun: string -> unit
      passed: string -> TimeSpan -> unit
      ignored: string -> string -> unit
      failed: string -> string -> TimeSpan -> unit
      exn: string -> exn -> TimeSpan -> unit }

      static member Default = {
          beforeRun = ignore
          passed = ignore2
          ignored = ignore2
          failed = ignore3
          exn = ignore3
      }

  /// Runs a list of tests, with parameterized printers (progress indicators) and traversal.
  /// Returns list of results.
  let evalTestList =
      let failExceptions = [
          typeof<AssertException>.AssemblyQualifiedName
          "NUnit.Framework.AssertionException, NUnit.Framework"
          "NUnit.Framework.AssertionException, nunit.framework"
          "Gallio.Framework.Assertions.AssertionFailureException, Gallio"
          "Gallio.Framework.Assertions.AssertionException, Gallio"
          "Xunit.Sdk.AssertException, Xunit"
      ]
      let ignoreExceptions = [
          "NUnit.Framework.IgnoreException, NUnit.Framework"
          "NUnit.Framework.IgnoreException, nunit.framework"
          typeof<IgnoreException>.AssemblyQualifiedName
      ]
      let failExceptionTypes = lazy List.choose Type.TryGetType failExceptions
      let ignoreExceptionTypes = lazy List.choose Type.TryGetType ignoreExceptions

      let (|ExceptionInList|_|) (l: Type list) (e: #exn) =
          let et = e.GetType()
          if l |> List.exists (fun x -> x.IsAssignableFrom et)
              then Some()
              else None

      fun (printers: TestPrinters) map ->
          let execOne (name: string, test) =
              printers.beforeRun name
              let w = System.Diagnostics.Stopwatch.StartNew()
              try
                  test()
                  w.Stop()
                  printers.passed name w.Elapsed
                  { name = name
                    result = Passed
                    time = w.Elapsed }
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
                      printers.failed name msg w.Elapsed
                      { name = name
                        result = Failed msg
                        time = w.Elapsed }
                  | ExceptionInList ignoreExceptionTypes.Value ->
                      printers.ignored name e.Message
                      { name = name
                        result = Ignored e.Message
                        time = w.Elapsed }
                  | _ ->
                      printers.exn name e w.Elapsed
                      { name = name
                        result = TestResult.Error e
                        time = w.Elapsed }
          map execOne

  /// Runs a tree of tests, with parameterized printers (progress indicators) and traversal.
  /// Returns list of results.
  let eval (printer: TestPrinters) map tests =
      Test.toTestCodeList tests
      |> evalTestList printer map
      |> Seq.toList

  let printFailed = tprintf "%s: Failed: %s (%A)\n"
  let printException name ex = tprintf "%s: Exception: %s (%A)\n" name (exnToString ex)

  /// Evaluates tests sequentially
  let evalSeq =
      let printer =
          { TestPrinters.Default with
              failed = printFailed
              exn = printException }

      eval printer Seq.map

  let pmap (f: _ -> _) (s: _ seq) = s.AsParallel().Select(f) :> _ seq

  /// Evaluates tests in parallel
  let evalPar =
      let funLock =
          let locker = obj()
          lock locker
      let inline funLock3 f a b c = funLock (fun () -> f a b c)
      let printFailed = funLock3 printFailed
      let printException = funLock3 printException
      let printer =
          { TestPrinters.Default with
              failed = printFailed
              exn = printException }
      eval printer pmap

  /// Runs tests, returns error code
  let runEval eval (tests: Test) =
      let w = System.Diagnostics.Stopwatch.StartNew()
      let results = eval tests
      w.Stop()
      let summary = { sumTestResults results with time = w.Elapsed }
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

  /// Scan filtered tests marked with TestsAttribute from an assembly
  let testFromAssemblyWithFilter typeFilter (a: Assembly) =
      a.GetExportedTypes()
      |> Seq.filter typeFilter
      |> Seq.choose testFromType
      |> Seq.toList
      |> listToTestListOption

  /// Scan tests marked with TestsAttribute from an assembly
  let testFromAssembly = testFromAssemblyWithFilter (fun _ -> true)

  /// Scan tests marked with TestsAttribute from entry assembly
  let testFromThisAssembly () = testFromAssembly (Assembly.GetEntryAssembly())

[<AutoOpen; Extension>]
module Tests =
  open Impl
  open Helpers

  /// Fail this test
  let inline failtest msg = raise <| AssertException msg
  /// Fail this test
  let inline failtestf fmt = Printf.ksprintf (fun msg -> raise <| AssertException msg) fmt

  /// Skip this test
  let inline skiptest msg = raise <| IgnoreException msg
  /// Skip this test
  let inline skiptestf fmt = Printf.ksprintf (fun msg -> raise <| IgnoreException msg) fmt

  /// Builds a list/group of tests
  let inline testList name tests = TestLabel(name, TestList tests)

  /// Builds a test case
  let inline testCase name test = TestLabel(name, TestCase test)

  /// Applies a function to a list of values to build test cases
  let inline testFixture setup =
        Seq.map (fun (name, partialTest) ->
                      testCase name (setup partialTest))

  /// Applies a value to a list of partial tests
  let inline testParam param =
        Seq.map (fun (name, partialTest) ->
                      testCase name (partialTest param))

  type TestCaseBuilder(name) =
      member x.TryFinally(f, compensation) =
          try
              f()
          finally
              compensation()
      member x.TryWith(f, catchHandler) =
          try
              f()
          with e -> catchHandler e
      member x.Using(disposable: #IDisposable, f) =
          try
              f disposable
          finally
              match disposable with
              | null -> ()
              | disp -> disp.Dispose()
      member x.For(sequence, f) =
          for i in sequence do f i
      member x.Combine(f1, f2) = f2(); f1
      member x.Zero() = ()
      member x.Delay f = f
      member x.Run f = testCase name f

  let inline test name =
    TestCaseBuilder name

  /// Runs the passed tests
  let run tests =
    runEval evalSeq tests

  /// Runs tests in parallel
  let runParallel tests = runEval evalPar tests

  // Runner options
  type RunOptions = { parallel: bool }

  /// Parses command-line arguments
  let parseArgs =
    let defaultOptions = { RunOptions.parallel = false }
    let opts = [ "/m", fun o -> { o with RunOptions.parallel = true } ]
    fun (args: string[]) ->
      (defaultOptions, args)
      ||> Seq.fold (fun opt arg ->
            (opt, opts) ||> Seq.fold (fun o (a,f) -> if a = arg then f o else o))

  /// Runs tests with supplied options. Returns 0 if all tests passed, otherwise 1
  let defaultMainWithOptions tests (options: RunOptions) =
    let run = if options.parallel then runParallel else run
    run tests

  /// Runs tests with supplied command-line options. Returns 0 if all tests passed, otherwise 1
  let defaultMain tests =
    parseArgs >> defaultMainWithOptions tests

  /// Runs tests in this assembly with supplied command-line options. Returns 0 if all tests passed, otherwise 1
  let defaultMainThisAssembly args =
    let tests =
      match testFromAssembly (Assembly.GetEntryAssembly()) with
      | Some t -> t
      | None -> TestList []
    defaultMain tests args

  /// Runs tests in this assembly with supplied command-line options.
  /// You may also pass a filter that selected a subset of tests to run.
  /// Returns 0 if all tests passed, otherwise 1
  let defaultMainThisAssemblyFilter args filter =
    let tests =
      match testFromAssembly (Assembly.GetEntryAssembly()) with
      | Some t -> filter t
      | None -> TestList []
    defaultMain tests args
