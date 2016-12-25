namespace Expecto

#nowarn "46"

open System
open System.Linq
open System.Runtime.CompilerServices
open System.Reflection

/// Actual test function
type TestCode = unit -> unit

/// The parent state (watching the tests as a tree structure) will influence
/// the child tests state. By following rules, if parent test state is:
///     - Focused will elevate all Normal child tests to Focused.
///              Focused and Pending child tests will not change state(behavior)
///     - Normal will not influence the child tests states(behavior).
///     - Pending will elevate all Normal and Focused child tests to Pending.
///              Pending child test will not change state(behavior)
type FocusState =
  /// The default state of a test that will be evaluated
  | Normal
  /// The state of a test that will be ignored by Expecto
  | Pending
  /// The state of a test that will be evaluated
  /// All other test marked with Normal or Pending will be ignored
  | Focused

  with static member isFocused = function | Focused -> true | _ -> false

/// Test tree – this is how you compose your tests as values. Since
/// any of these can act as a test, you can pass any of these DU cases
/// into a function that takes a Test.
type Test =
  /// A test case is a function from unit to unit, that can be executed
  /// by Expecto to run the test code.
  | TestCase of code:TestCode * state:FocusState
  /// A collection/list of tests.
  | TestList of tests:Test seq * state:FocusState
  /// A labelling of a Test (list or test code).
  | TestLabel of label:string * test:Test * state:FocusState

type ExpectoException(msg) = inherit Exception(msg)
type AssertException(msg) = inherit ExpectoException(msg)
type IgnoreException(msg) = inherit ExpectoException(msg)

/// Marks a top-level test for scanning
/// The test will run even if PTest is also present.
[<AttributeUsage(AttributeTargets.Method ||| AttributeTargets.Property ||| AttributeTargets.Field)>]
type TestsAttribute() = inherit Attribute()

/// Allows to mark a test as Pending (will be skipped/ignored if no other TestAttribute is present)
/// Is a fast way to exclude some tests from running.
/// If FTest or Test is also present then this attribute will be ignored.
[<AttributeUsage(AttributeTargets.Method ||| AttributeTargets.Property ||| AttributeTargets.Field)>]
type PTestsAttribute() = inherit Attribute()

/// Allows to mark a test as FocusState.Focused (will be runned and will change the behavior for
/// all other tests marked as FocusState.Normal to be ignored)
/// Is a fast way to exclude some tests from running.
/// The test will run even if PTest is also present. Have priority over TestAttribute.
[<AttributeUsage(AttributeTargets.Method ||| AttributeTargets.Property ||| AttributeTargets.Field)>]
type FTestsAttribute() = inherit Attribute()

module Helpers =

  let fst3 (a,_,_) = a
  let snd3 (_,b,_) = b
  let trd3 (_,_,c) = c

  let inline ignore2 _ = ignore
  let inline ignore3 _ = ignore2

  let bracket setup teardown f () =
    let v = setup()
    try
      f v
    finally
      teardown v

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

  let matchFocusAttributes = function
    | "Expecto.FTestsAttribute" -> Some  (1, Focused)
    | "Expecto.TestsAttribute" -> Some (2, Normal)
    | "Expecto.PTestsAttribute" -> Some (3, Pending)
    | _ -> None

  let allTestAttributes = Set.ofList [  (typeof<FTestsAttribute>).FullName
                                        (typeof<TestsAttribute>).FullName
                                        (typeof<PTestsAttribute>).FullName]

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

    member m.MatchTestsAttributes () =
      m.GetCustomAttributes true
      |> Array.map (fun t -> t.GetType().FullName)
      |> Set.ofArray
      |> Set.intersect allTestAttributes
      |> Set.toList
      |> List.choose matchFocusAttributes
      |> List.sortBy fst
      |> List.map snd
      |> List.tryFind (fun _ -> true)

[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module Test =
  open Helpers
  /// Compute the child test state based on parent test state
  let computeChildFocusState parentState childState =
    match parentState, childState with
    | Focused, Pending -> Pending
    | Pending, _ -> Pending
    | Focused, _ -> Focused
    | Normal, _ -> childState

  /// Flattens a tree of tests
  let toTestCodeList =
    let rec loop parentName testList parentState =
      function
      | TestLabel (name, test, state) ->
        let fullName =
          if String.IsNullOrEmpty parentName
            then name
            else parentName + "/" + name
        loop fullName testList (computeChildFocusState parentState state) test
      | TestCase (test, state) -> Seq.cons (parentName, test, (computeChildFocusState parentState state)) testList
      | TestList (tests, state) -> Seq.collect (loop parentName testList (computeChildFocusState parentState state)) tests
    loop null Seq.empty Normal

  /// Recursively maps all TestCodes in a Test
  let rec wrap f =
    function
    | TestCase (test, state) -> TestCase ((f test), state)
    | TestList (testList, state) -> TestList ((Seq.map (wrap f) testList), state)
    | TestLabel (label, test, state) -> TestLabel (label, wrap f test, state)

  /// Enforce a FocusState on a test by replacing the current state
  /// Is not used (against YAGNI), but is here to make it clear for intellisense discovery
  /// that the translateFocusState is not intended as replacement
  let replaceFocusState newFocusState =
    function
    | TestCase (test, _) -> TestCase(test, newFocusState)
    | TestList (testList, _) -> TestList(testList, newFocusState)
    | TestLabel (label, test, _) -> TestLabel(label, test, newFocusState)

  /// Change the FocusState by appling the old state to a new state
  /// Note: this is not state replacement!!!
  /// Used in replaceTestCode and the order is intended for scenario:
  ///  1. User wants to automate some tests and his intent is not to change
  ///      the test state (use Normal), so this way the current state will be preserved
  /// Don't see the use case: the user wants to automate some tests and wishes
  /// to change the test states
  let translateFocusState newFocusState =
    function
    | TestCase (test, oldFocusState) -> TestCase(test, computeChildFocusState oldFocusState newFocusState)
    | TestList (testList, oldFocusState) -> TestList(testList, computeChildFocusState oldFocusState newFocusState)
    | TestLabel (label, test, oldFocusState) -> TestLabel(label, test, computeChildFocusState oldFocusState newFocusState)


  /// Recursively replaces TestCodes in a Test
  /// Check translateFocusState for focus state behavior description
  let rec replaceTestCode (f:string -> TestCode -> Test) =
    function
    | TestLabel (label, TestCase (test, childState), parentState) ->
          f label test
          |> translateFocusState (computeChildFocusState parentState childState)
    | TestCase (test, state) ->
          f null test
          |> translateFocusState state
    | TestList (testList, state) -> TestList (Seq.map (replaceTestCode f) testList, state)
    | TestLabel (label, test, state) -> TestLabel (label, replaceTestCode f test, state)

  /// Filter tests by name
  let filter pred =
    toTestCodeList
    >> Seq.filter (fst3 >> pred)
    >> Seq.map (fun (name, test, state) -> TestLabel (name, TestCase (test, state), state))
    >> (fun x -> TestList (x,Normal))

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
  open Expecto.Logging
  open Expecto.Logging.Message
  open Helpers

  let logger = Log.create "Expecto"

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

  [<StructuredFormatDisplay("{description}")>]
  type TestResultSummary =
    { passed   : string list
      ignored  : string list
      failed   : string list
      errored  : string list
      duration : TimeSpan }

      member x.total =
        x.passed.Length + x.ignored.Length + x.failed.Length

      override x.ToString() =
        sprintf "%d tests run: %d passed, %d ignored, %d failed, %d errored (%A)\n"
                (x.errored.Length + x.failed.Length + x.passed.Length)
                x.passed.Length x.ignored.Length x.failed.Length x.errored.Length x.duration
      member x.description =
        x.ToString()
      static member (+) (c1: TestResultSummary, c2: TestResultSummary) =
        { passed = c1.passed @ c2.passed
          ignored = c1.ignored @ c2.ignored
          failed = c1.failed @ c2.failed
          errored = c1.errored @ c2.errored
          duration = c1.duration + c2.duration }

      static member errorCode (c: TestResultSummary) =
        (if c.failed.Length > 0 then 1 else 0) ||| (if c.errored.Length > 0 then 2 else 0)

  [<StructuredFormatDisplay("{description}")>]
  type TestRunResult =
   { name     : string
     result   : TestResult
     duration : TimeSpan }
    override x.ToString() =
     sprintf "%s: %s (%A)" x.name (x.result.ToString()) x.duration
    member x.description = x.ToString()
    static member isPassed (r: TestRunResult) = TestResult.isPassed r.result
    static member isIgnored (r: TestRunResult) = TestResult.isIgnored r.result
    static member isFailed (r: TestRunResult) = TestResult.isFailed r.result
    static member isException (r: TestRunResult) = TestResult.isException r.result
    static member isFailedOrException r = TestRunResult.isFailed r || TestRunResult.isException r

  let sumTestResults (results: #seq<TestRunResult>) =
    let counts =
      results
      |> Seq.groupBy (fun r -> TestResult.tag r.result )
      |> dict

    let get result =
      match counts.TryGetValue (TestResult.tag result) with
      | true, v -> v |> Seq.map (fun r -> r.name) |> Seq.toList
      | _ -> List.empty

    { passed   = get TestResult.Passed
      ignored  = get (TestResult.Ignored "")
      failed   = get (TestResult.Failed "")
      errored  = get (TestResult.Error null)
      duration = results |> Seq.map (fun r -> r.duration) |> Seq.fold (+) TimeSpan.Zero }

  let logSummary (summary : TestResultSummary) =
    let passed = (summary.passed |> String.concat "\n\t")
    let ignored = (summary.ignored |> String.concat "\n\t")
    let failed = (summary.failed |> String.concat "\n\t")
    let errored = (summary.errored |> String.concat "\n\t")

    logger.info (
      eventX "EXPECTO?! Summary...\nPassed:\n\t{passed}\nIgnored:\n\t{ignored}\nFailed:\n\t{failed}\nErrored:\n\t{errored} "
      >> setField "passed" passed
      >> setField "ignored" ignored
      >> setField "failed" failed
      >> setField "errored" errored)



  /// Hooks to print report through test run
  type TestPrinters =
    { /// Called before a test run (e.g. at the top of your main function)
      beforeRun: Test -> unit
      /// Called before atomic test (TestCode) is executed.
      beforeEach: string -> unit
      /// test name -> time taken -> unit
      passed: string -> TimeSpan -> unit
      /// test name -> ignore message -> unit
      ignored: string -> string -> unit
      /// test name -> other message -> time taken -> unit
      failed: string -> string -> TimeSpan -> unit
      /// test name -> exception -> time taken -> unit
      exn: string -> exn -> TimeSpan -> unit
      /// Prints a summary given the test result counts
      summary : TestResultSummary -> unit }

    static member Default =
      { beforeRun = fun _tests ->
          logger.info (
            eventX "EXPECTO? Running tests...")

        beforeEach = fun n ->

          logger.debug (
            eventX "{testName} starting..."
            >> setField "testName" n)
        passed = fun n d ->
          logger.debug (
            eventX "{testName} passed in {duration}."
            >> setField "testName" n
            >> setField "duration" d)
        ignored = fun n m ->
          logger.warn (
            eventX "{testName} was ignored. {reason}"
            >> setField "testName" n
            >> setField "reason" m)
        failed = fun n m d ->
          logger.error (
            eventX "{testName} failed in {duration}. {message}"
            >> setField "testName" n
            >> setField "duration" d
            >> setField "message" m)
        exn = fun n e d ->
          logger.error (
            eventX "{testName} errored in {duration}"
            >> setField "testName" n
            >> setField "duration" d
            >> addExn e)
        summary = fun summary ->
          logger.info (
            eventX "EXPECTO! {total} tests run in {duration} – {passes} passed, {ignores} ignored, {failures} failed, {errors} errored."
            >> setField "total" summary.total
            >> setField "duration" summary.duration
            >> setField "passes" summary.passed.Length
            >> setField "ignores" summary.ignored.Length
            >> setField "failures" summary.failed.Length
            >> setField "errors" summary.errored.Length)}
    static member Summary =
      { TestPrinters.Default with
          summary = fun summary ->
            TestPrinters.Default.summary summary
            logSummary summary }

    type WrappedFocusedState =
      | Enabled of state:FocusState
      | UnFocused of state:FocusState

      with
        /// Used to check if a test should be run and to generate a proper status messsage
        member x.ShouldSkipEvaluation =
          match x with
          | UnFocused Focused -> failwith "Should never reach this state - this is a bug in Expecto - please let us know"
          | UnFocused Pending
          | Enabled Pending -> Some "The test or one of his parents is marked as Pending"
          | UnFocused _ -> Some "The test is skiped because other tests are Focused"
          | Enabled _-> None

        /// tests: seq<string * TestCode * FocusState>) -> seq<string * TestCode * WrappedFocusedState>
        static member WrapStates tests =
          let applyFocusedWrapping = function
            | Focused -> Enabled Focused
            | a -> UnFocused a
          let testsList = tests |> Seq.toList
          let existsFocusedTests = testsList |> Seq.exists (trd3 >> (FocusState.isFocused))
          let wrappingMethod = if existsFocusedTests then applyFocusedWrapping else Enabled
          testsList |> Seq.map (fun (n, t, s) -> (n, t, wrappingMethod s))

  // here Anthony:
  //  For 'normal tests';
  //  readerWriterSlim.EnterRead() (perhaps?)
  //  For 'perf tests';
  // readerWriterSlim.EnterWrite()
  let enterLock _ _ = ()
  // need to create lease module, implement these functions.
  // The first parameter is the state (the Map<Name*Test, RWLS> perhaps?)
  // the second is the key to enter a lock for. Should allow for multiple
  // 'normal' and untagged tests (that can run parallel = readers) and then
  // some 'sequential' tests like the performance tests, benchmark tests or
  // the Expect.isFasterThan usage inside a given test.

  (*
    Equivalent and opposite of above ...
    readerWriterSlim.Exit(...)
  *)
  let exitLock _ _ = ()

  /// Runs a list of tests, with parameterized printers (progress indicators) and traversal.
  /// Returns list of results.
  let evalTestList =
    let failExceptions = [
      typeof<AssertException>.AssemblyQualifiedName
    ]
    let ignoreExceptions = [
      typeof<IgnoreException>.AssemblyQualifiedName
    ]
    let failExceptionTypes = lazy List.choose Type.TryGetType failExceptions
    let ignoreExceptionTypes = lazy List.choose Type.TryGetType ignoreExceptions

    let (|ExceptionInList|_|) (l: Type list) (e: #exn) =
      let et = e.GetType()
      if l |> List.exists (fun x -> x.IsAssignableFrom et) then
        Some()
      else
        None

    fun (printers: TestPrinters) (* locks : Locks *) map ->

      let beforeOne (name: string, test, wrappedFocusedState) =
        printers.beforeEach name
        name, test, wrappedFocusedState

      let execFocused next (name, test, wrappedFocusedState : WrappedFocusedState) =
        match wrappedFocusedState.ShouldSkipEvaluation with
        | Some ignoredMessage ->
          { name     = name
            result   = Ignored ignoredMessage
            duration = TimeSpan.Zero }
        | _ ->
          next (name, test, wrappedFocusedState)

      /// Used to constrain parallelism for tests that cannot be run parallel
      /// with correct results, such as Expect.isFasterThan or `benchmark<..>`
      /// based tests. Note; this does not give an ordering guarantee.
      let maybeSequence next (name, test, wrappedFocusedState) =
        try
          // here Anthony
          enterLock () (*locks*) (name, test)
          next (name, test, wrappedFocusedState)
        finally
          exitLock () (*locks*) (name, test)

      let execOne (name: string, test: TestCode, wrappedFocusedState: WrappedFocusedState) =
        let w = System.Diagnostics.Stopwatch.StartNew()
        try
          test ()
          w.Stop()
          { name     = name
            result   = Passed
            duration = w.Elapsed }
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
            { name     = name
              result   = Failed msg
              duration = w.Elapsed }
          | ExceptionInList ignoreExceptionTypes.Value ->
            { name     = name
              result   = Ignored e.Message
              duration = w.Elapsed }
          | _ ->
            { name     = name
              result   = TestResult.Error e
              duration = w.Elapsed }

      let printOne (trr : TestRunResult) =
        match trr.result with
        | Passed -> printers.passed trr.name trr.duration
        | Failed message -> printers.failed trr.name message trr.duration
        | Ignored message -> printers.ignored trr.name message
        | Error e -> printers.exn trr.name e trr.duration
        trr

      let pipeline =
        beforeOne
        >> execFocused (maybeSequence execOne)
        >> printOne

      WrappedFocusedState.WrapStates
      >> (map pipeline)

  /// Runs a tree of tests, with parameterized printers (progress indicators) and traversal.
  /// Returns list of results.
  let eval (printer: TestPrinters) map tests =
    Test.toTestCodeList tests
    |> evalTestList printer map
    |> Seq.toList

  /// Evaluates tests sequentially
  let evalSeq =
    let printer =
      TestPrinters.Default

    eval printer Seq.map

  let pmap (f: _ -> _) (s: _ seq) = s.AsParallel().Select(f) :> _ seq

  /// Evaluates tests in parallel
  let evalPar =
    let printer =
      TestPrinters.Default

    eval printer pmap

  /// Runs tests, returns error code
  let runEval printer eval (tests: Test) =
    printer.beforeRun tests

    let w = System.Diagnostics.Stopwatch.StartNew()
    let results = eval tests
    w.Stop()
    let summary = { sumTestResults results with duration = w.Elapsed }
    printer.summary summary

    TestResultSummary.errorCode summary

  let testFromMember (mi: MemberInfo): Test option  =
    let getTestFromMemberInfo focusedState =
      match box mi with
      | :? FieldInfo as m ->
        if m.FieldType = typeof<Test>
        then Some(focusedState, unbox (m.GetValue(null)))
        else None
      | :? MethodInfo as m ->
        if m.ReturnType = typeof<Test>
        then Some(focusedState, unbox (m.Invoke(null, null)))
        else None
      | :? PropertyInfo as m ->
        if m.PropertyType = typeof<Test>
        then Some(focusedState, unbox (m.GetValue(null, null)))
        else None
      | _ -> None
    mi.MatchTestsAttributes ()
    |> Option.map getTestFromMemberInfo
    |> function
    | Some (Some (focusedState, test)) -> Some (Test.translateFocusState focusedState test)
    | _ -> None

  let listToTestListOption =
    function
    | [] -> None
    | x -> Some (TestList (x, Normal))

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
  open Argu
  open Expecto.Logging

  /// Fail this test
  let inline failtest msg = raise <| AssertException msg
  /// Fail this test
  let inline failtestf fmt = Printf.ksprintf (fun msg -> raise <| AssertException msg) fmt

  /// Skip this test
  let inline skiptest msg = raise <| IgnoreException msg
  /// Skip this test
  let inline skiptestf fmt = Printf.ksprintf (fun msg -> raise <| IgnoreException msg) fmt

  /// Builds a list/group of tests that will be ignored by Expecto if exists focused tests and none of the parents is focused
  let inline testList name tests = TestLabel(name, TestList (tests, Normal), Normal)
  /// Builds a list/group of tests that will make Expecto to ignore other unfocused tests
  let inline ftestList name tests = TestLabel(name, TestList (tests, Focused), Focused)
  /// Builds a list/group of tests that will be ignored by Expecto
  let inline ptestList name tests = TestLabel(name, TestList (tests, Pending), Pending)

  /// Builds a test case that will be ignored by Expecto if exists focused tests and none of the parents is focused
  let inline testCase name test = TestLabel(name, TestCase (test,Normal), Normal)
  /// Builds a test case that will make Expecto to ignore other unfocused tests
  let inline ftestCase name test = TestLabel(name, TestCase (test, Focused), Focused)
  /// Builds a test case that will be ignored by Expecto
  let inline ptestCase name test = TestLabel(name, TestCase (test, Pending), Pending)

  /// Applies a function to a list of values to build test cases
  let inline testFixture setup =
    Seq.map (fun (name, partialTest) ->
      testCase name (setup partialTest))

  /// Applies a value to a list of partial tests
  let inline testParam param =
    Seq.map (fun (name, partialTest) ->
      testCase name (partialTest param))

  type TestCaseBuilder(name, focusState) =
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
    member x.Run f =
      match focusState with
      | Normal -> testCase name f
      | Focused -> ftestCase name f
      | Pending -> ptestCase name f

  let inline test name =
    TestCaseBuilder (name, Normal)
  let inline ftest name =
    TestCaseBuilder (name, Focused)
  let inline ptest name =
    TestCaseBuilder (name, Pending)

  /// Runs the passed tests
  let run printer tests =
    runEval printer evalSeq tests

  /// Runs tests in parallel
  let runParallel printer tests =
    runEval printer evalPar tests

  // Runner options
  type ExpectoConfig =
    { /// Whether to run the tests in parallel. Defaults to
      /// true, because your code should not mutate global
      /// state by default.
      parallel : bool
      /// An optional filter function. Useful if you only would
      /// like to run a subset of all the tests defined in your assembly.
      filter   : Test -> Test
      /// Allows the test printer to be parametised to your liking.
      printer : TestPrinters
      /// Verbosity level (default: Info)
      verbosity : LogLevel
    }

  /// The default configuration for Expecto.
  let defaultConfig =
    { parallel  = true
      filter    = id
      printer   = TestPrinters.Default
      verbosity = LogLevel.Info }

  type CLIArguments =
    | Sequenced
    | Parallel
    | Debug
    | Filter of hiera:string
    | Filter_Test_List of substring:string
    | Filter_Test_Case of substring:string
    | Run of tests:string list
    | List_Tests
    | Summary

    interface IArgParserTemplate with
      member s.Usage =
        match s with
        | Sequenced -> "Don't run the tests in parallel."
        | Parallel -> "Run all tests in parallel (default)."
        | Debug -> "Extra verbose printing. Useful to combine with --sequenced."
        | Filter _ -> "Filter the list of tests by a hierarchy that's slash (/) separated."
        | Filter_Test_List _ -> "Filter the list of test lists by a substring."
        | Filter_Test_Case _ -> "Filter the list of test cases by a substring."
        | Run _ -> "Run only provided tests"
        | List_Tests -> "Doesn't run tests, print out list of tests instead"
        | Summary -> "Prints out summary after all tests are finished"

  [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
  module ExpectoConfig =

    /// Parses command-line arguments into a config. This allows you to
    /// override the config from the command line, rather than having
    /// to go into the compiled code to change how they are being run.
    /// Also checks if tests should be run or only listed
    let fillFromArgs baseConfig =
      let parser = ArgumentParser.Create<CLIArguments>()
      let flip f a b = f b a

      let getTestList (s : string) =
        let all = s.Split ('/')
        match all with
        | [||] -> [||]
        | [|x|] -> [||]
        | xs -> xs.[0 .. all.Length - 2]

      let getTastCase (s : string) =
        let all = s.Split ('/')
        match all with
        | [||] -> ""
        | xs -> xs.Last()



      let reduceKnown : CLIArguments -> (_ -> ExpectoConfig) =
        function
        | Sequenced -> fun o -> { o with ExpectoConfig.parallel = false }
        | Parallel -> fun o -> { o with parallel = true }
        | Debug -> fun o -> { o with verbosity = LogLevel.Debug }
        | Filter hiera -> fun o -> {o with filter = Test.filter (fun s -> s.StartsWith hiera )}
        | Filter_Test_List name ->  fun o -> {o with filter = Test.filter (fun s -> s |> getTestList |> Array.exists(fun s -> s.Contains name )) }
        | Filter_Test_Case name ->  fun o -> {o with filter = Test.filter (fun s -> s |> getTastCase |> fun s -> s.Contains name )}
        | Run tests -> fun o -> {o with filter = Test.filter (fun s -> tests |> List.exists ((=) s) )}
        | List_Tests -> id
        | Summary -> fun o -> {o with printer = TestPrinters.Summary}



      fun (args: string[]) ->
        let parsed =
          parser.Parse(
            args,
            ignoreMissing = true,
            ignoreUnrecognized = true,
            raiseOnUsage = false)
        let isList = parsed.Contains <@ List_Tests @>
        (baseConfig, parsed.GetAllResults()) ||> Seq.fold (flip reduceKnown), isList

  /// Prints out names of all tests for given test suite.
  let listTests test =
    test
    |> Test.toTestCodeList
    |> Seq.iter (fst3 >> printfn "%s")

  /// Runs tests with supplied options. Returns 0 if all tests passed, =
  /// otherwise 1
  let runTests config tests =
    let run = if config.parallel then runParallel else run
    Global.initialiseIfDefault
      { Global.defaultConfig with
          getLogger = fun name -> LiterateConsoleTarget(name, config.verbosity) :> Logger }
    run config.printer tests

  /// Runs tests in this assembly with supplied command-line options. Returns 0 if all tests passed, otherwise 1
  let runTestsInAssembly config args =
    let tests =
      match testFromAssembly (Assembly.GetEntryAssembly()) with
      | Some t -> t
      | None -> TestList ([], Normal)
    let config, isList = args |> ExpectoConfig.fillFromArgs config
    let tests = tests |> config.filter
    if isList then listTests tests; 0 else runTests config tests