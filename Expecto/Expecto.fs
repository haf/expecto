namespace Expecto

#nowarn "46"

open System
open System.Linq
open System.Runtime.CompilerServices
open System.Reflection
open System.Diagnostics
open System.Threading

// TODO: move to internal
type SourceLocation =
  { sourcePath : string
    lineNumber : int }
with
  static member empty =
    { sourcePath = ""
      lineNumber = 0 }

/// Actual test function; either an async one, or a synchronous one.
type TestCode =
  | Sync of stest:(unit -> unit)
  | Async of atest:Async<unit>

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
  | TestList of tests:Test list * state:FocusState
  /// A labelling of a Test (list or test code).
  | TestLabel of label:string * test:Test * state:FocusState
  /// Require sequenced for a Test (list or test code).
  | Sequenced of Test

// TODO: move to internal namespace
type ExpectoException(msg) = inherit Exception(msg)
// TODO: move to internal namespace
type AssertException(msg) = inherit ExpectoException(msg)
// TODO: move to internal namespace
type FailedException(msg) = inherit ExpectoException(msg)
// TODO: move to internal namespace
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

/// Allows to mark a test as FocusState.Focused (will be run and will change the behavior for
/// all other tests marked as FocusState.Normal to be ignored)
/// Is a fast way to exclude some tests from running.
/// The test will run even if PTest is also present. Have priority over TestAttribute.
[<AttributeUsage(AttributeTargets.Method ||| AttributeTargets.Property ||| AttributeTargets.Field)>]
type FTestsAttribute() = inherit Attribute()

// TODO: make internal
module Async =
  // TODO: make internal
  let map fn a =
    async {
      let! v = a
      return fn v
    }

  // TODO: make internal
  let bind fn a =
    async.Bind(a, fn)

  /// Traverses the list of async, spawning them with Async.Start and
  let parallelLimit maxParallelism s =
    let workers = List.length s |> min maxParallelism
    let handle = new ManualResetEventSlim(false)
    let mutable results = []
    let mb =
      MailboxProcessor.Start(fun mb ->
        let rec loop queue count =
          async {
            // by receiving one by one, we ensure we can mutate `results`
            let! msg = mb.Receive()
            match msg with
            // None acts as a kanban card – running another one
            | None ->
              match queue with
              | asyncWork :: newQueue ->
                async {
                  try
                    let! r = asyncWork
                    Some r |> mb.Post
                  finally
                    mb.Post None
                } |> Async.Start
                return! loop newQueue count

              | [] ->
                if count = 1 then
                  handle.Set()
                  (mb :> IDisposable).Dispose()
                else
                  return! loop queue (count-1)

            // the other side of the coin; receiving results
            | Some r ->
              results <- r :: results
              return! loop queue count
          }
        loop s workers)

    let rec start n =
      if n > 0 then
        mb.Post None
        start (n-1)

    start workers
    Async.AwaitWaitHandle handle.WaitHandle
    |> map (fun _ -> results)

// TODO: make internal
module Helpers =

  let inline fst3 (a,_,_) = a
  let inline snd3 (_,b,_) = b
  let inline trd3 (_,_,c) = c

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

  module Option =
    let orDefault def =
      function | Some a -> a | None -> def

  type Type with
    static member TryGetType t =
      try
        Type.GetType(t, true) |> Some
      with _ ->
        None

  let matchFocusAttributes = function
    | "Expecto.FTestsAttribute" -> Some (1, Focused)
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

// TODO: move to internal
type FlatTest =
  { name      : string
    test      : TestCode
    state     : FocusState
    sequenced : bool }

[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module Test =
  /// Compute the child test state based on parent test state
  let computeChildFocusState parentState childState =
    match parentState, childState with
    | Focused, Pending -> Pending
    | Pending, _ -> Pending
    | Focused, _ -> Focused
    | Normal, _ -> childState

  /// Flattens a tree of tests
  let toTestCodeList =
    let rec loop parentName testList parentState sequenced =
      function
      | TestLabel (name, test, state) ->
        let fullName =
          if String.IsNullOrEmpty parentName
            then name
            else parentName + "/" + name
        loop fullName testList (computeChildFocusState parentState state) sequenced test
      | TestCase (test, state) -> {name=parentName; test=test; state=computeChildFocusState parentState state; sequenced=sequenced} :: testList
      | TestList (tests, state) -> List.collect (loop parentName testList (computeChildFocusState parentState state) sequenced) tests
      | Sequenced test -> loop parentName testList parentState true test
    loop null [] Normal false

  /// Recursively maps all TestCodes in a Test
  let rec wrap f =
    function
    | TestCase (test, state) -> TestCase ((f test), state)
    | TestList (testList, state) -> TestList (testList |> List.map (wrap f), state)
    | TestLabel (label, test, state) -> TestLabel (label, wrap f test, state)
    | Sequenced test -> Sequenced (wrap f test)

  /// Enforce a FocusState on a test by replacing the current state
  /// Is not used (against YAGNI), but is here to make it clear for intellisense discovery
  /// that the translateFocusState is not intended as replacement
  let rec replaceFocusState newFocusState =
    function
    | TestCase (test, _) -> TestCase(test, newFocusState)
    | TestList (testList, _) -> TestList(testList, newFocusState)
    | TestLabel (label, test, _) -> TestLabel(label, test, newFocusState)
    | Sequenced test -> Sequenced (replaceFocusState newFocusState test)

  /// Change the FocusState by appling the old state to a new state
  /// Note: this is not state replacement!!!
  /// Used in replaceTestCode and the order is intended for scenario:
  ///  1. User wants to automate some tests and his intent is not to change
  ///      the test state (use Normal), so this way the current state will be preserved
  /// Don't see the use case: the user wants to automate some tests and wishes
  /// to change the test states
  let rec translateFocusState newFocusState =
    function
    | TestCase (test, oldFocusState) -> TestCase(test, computeChildFocusState oldFocusState newFocusState)
    | TestList (testList, oldFocusState) -> TestList(testList, computeChildFocusState oldFocusState newFocusState)
    | TestLabel (label, test, oldFocusState) -> TestLabel(label, test, computeChildFocusState oldFocusState newFocusState)
    | Sequenced test -> Sequenced (translateFocusState newFocusState test)

  /// Recursively replaces TestCodes in a Test.
  /// Check translateFocusState for focus state behaviour description.
  let rec replaceTestCode (f:string -> TestCode -> Test) =
    function
    | TestLabel (label, TestCase (test, childState), parentState) ->
      f label test
      |> translateFocusState (computeChildFocusState parentState childState)
    | TestCase (test, state) ->
      f null test
      |> translateFocusState state
    | TestList (testList, state) -> TestList (List.map (replaceTestCode f) testList, state)
    | TestLabel (label, test, state) -> TestLabel (label, replaceTestCode f test, state)
    | Sequenced test -> Sequenced (replaceTestCode f test)

  /// Filter tests by name.
  let filter pred =
    toTestCodeList
    >> List.filter (fun t -> pred t.name)
    >> List.map (fun t ->
        let test = TestLabel (t.name, TestCase (t.test, t.state), t.state)
        if t.sequenced then Sequenced test else test)
    >> (fun x -> TestList (x,Normal))

  /// Applies a timeout to a test.
  let timeout timeout (test: TestCode) : TestCode =
    async {
      try
        let test =
          match test with
          | Async test -> test
          | Sync test -> async { test() }

        let! async = Async.StartChild(test, timeout)
        do! async
      with :? TimeoutException ->
        let ts = TimeSpan.FromMilliseconds (float timeout)
        raise <| AssertException(sprintf "Timeout (%A)" ts)
    } |> Async

// TODO: make internal?
module Impl =
  open Expecto.Logging
  open Expecto.Logging.Message
  open Helpers
  open Mono.Cecil
  open Mono.Cecil.Rocks

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
    { passed   : TestRunResult list
      ignored  : TestRunResult list
      failed   : TestRunResult list
      errored  : TestRunResult list
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


  and [<StructuredFormatDisplay("{description}")>] TestRunResult =
   { name     : string
     location : SourceLocation
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
      | true, v -> v |> Seq.toList
      | _ -> List.empty

    { passed   = get TestResult.Passed
      ignored  = get (TestResult.Ignored "")
      failed   = get (TestResult.Failed "")
      errored  = get (TestResult.Error null)
      duration = results |> Seq.map (fun r -> r.duration) |> Seq.fold (+) TimeSpan.Zero }

  let createSummaryMessage (summary: TestResultSummary) =
    let handleLineBreaks elements =
        elements
        |> List.map (fun n -> "\n\t" + n.name)
        |> String.concat ""

    let passed = summary.passed |> handleLineBreaks
    let passedCount = summary.passed |> List.length
    let ignored = summary.ignored |> handleLineBreaks
    let ignoredCount = summary.ignored |> List.length
    let failed = summary.failed |> handleLineBreaks
    let failedCount = summary.failed |> List.length
    let errored = summary.errored |> handleLineBreaks
    let erroredCount = summary.errored |> List.length

    let digits =
      [passedCount; ignoredCount; failedCount; erroredCount ]
      |> List.map (fun x -> x.ToString().Length)
      |> List.max

    let align (d:int) offset = d.ToString().PadLeft(offset + digits)

    eventX "EXPECTO?! Summary...\nPassed: {passedCount}{passed}\nIgnored: {ignoredCount}{ignored}\nFailed: {failedCount}{failed}\nErrored: {erroredCount}{errored}"
    >> setField "passed" passed
    >> setField "passedCount" (align passedCount 1)
    >> setField "ignored" ignored
    >> setField "ignoredCount" (align ignoredCount 0)
    >> setField "failed" failed
    >> setField "failedCount" (align failedCount 1)
    >> setField "errored" errored
    >> setField "erroredCount" (align erroredCount 0)

  let createSummaryText (summary: TestResultSummary) =
    createSummaryMessage summary Info
    |> Expecto.Logging.Formatting.defaultFormatter

  let logSummary (summary: TestResultSummary) =
    createSummaryMessage summary
    |> logger.logWithAck Info

  let logSummaryWithLocation (summary: TestResultSummary) =
    let handleLineBreaks elements =
      let format n = sprintf "%s [%s:%d]" n.name n.location.sourcePath n.location.lineNumber

      let text = elements |> Seq.map format |> String.concat "\n\t"
      if text = "" then text else text + "\n"

    let passed = summary.passed |> handleLineBreaks
    let passedCount = summary.passed |> List.length
    let ignored = summary.ignored |> handleLineBreaks
    let ignoredCount = summary.ignored |> List.length
    let failed = summary.failed |> handleLineBreaks
    let failedCount = summary.failed |> List.length
    let errored = summary.errored |> handleLineBreaks
    let erroredCount = summary.errored |> List.length

    let digits =
      [passedCount; ignoredCount; failedCount; erroredCount ]
      |> List.map (fun x -> x.ToString().Length)
      |> List.max

    let align (d:int) offset = d.ToString().PadLeft(offset + digits)

    logger.logWithAck Info (
      eventX "EXPECTO?! Summary...\nPassed: {passedCount}\n\t{passed}Ignored: {ignoredCount}\n\t{ignored}Failed: {failedCount}\n\t{failed}Errored: {erroredCount}\n\t{errored}"
      >> setField "passed" passed
      >> setField "passedCount" (align passedCount 1)
      >> setField "ignored" ignored
      >> setField "ignoredCount" (align ignoredCount 0)
      >> setField "failed" failed
      >> setField "failedCount" (align failedCount 1)
      >> setField "errored" errored
      >> setField "erroredCount" (align erroredCount 0))

  /// Hooks to print report through test run
  type TestPrinters =
    { /// Called before a test run (e.g. at the top of your main function)
      beforeRun: Test -> Async<unit>
      /// Called before atomic test (TestCode) is executed.
      beforeEach: string -> Async<unit>
      /// info
      info: string -> Async<unit>
      /// test name -> time taken -> unit
      passed: string -> TimeSpan -> Async<unit>
      /// test name -> ignore message -> unit
      ignored: string -> string -> Async<unit>
      /// test name -> other message -> time taken -> unit
      failed: string -> string -> TimeSpan -> Async<unit>
      /// test name -> exception -> time taken -> unit
      exn: string -> exn -> TimeSpan -> Async<unit>
      /// Prints a summary given the test result counts
      summary : TestResultSummary -> Async<unit> }

    static member silent =
      { beforeRun = fun _ -> async.Zero()
        beforeEach = fun _ -> async.Zero()
        info = fun _ -> async.Zero()
        passed = fun _ _ -> async.Zero()
        ignored = fun _ _ -> async.Zero()
        failed = fun _ _ _ -> async.Zero()
        exn = fun _ _ _ -> async.Zero()
        summary = fun _ -> async.Zero() }

    static member defaultPrinter =
      { beforeRun = fun _tests ->
          logger.logWithAck Info (eventX "EXPECTO? Running tests...")

        beforeEach = fun n ->
          logger.logWithAck Debug (
            eventX "{testName} starting..."
            >> setField "testName" n)

        info = fun s ->
          logger.logWithAck Info (eventX s)

        passed = fun n d ->
          logger.logWithAck Debug (
            eventX "{testName} passed in {duration}."
            >> setField "testName" n
            >> setField "duration" d)

        ignored = fun n m ->
          logger.logWithAck Debug (
            eventX "{testName} was ignored. {reason}"
            >> setField "testName" n
            >> setField "reason" m)

        failed = fun n m d ->
          logger.logWithAck LogLevel.Error (
            eventX "{testName} failed in {duration}. {message}"
            >> setField "testName" n
            >> setField "duration" d
            >> setField "message" m)

        exn = fun n e d ->
          logger.logWithAck LogLevel.Error (
            eventX "{testName} errored in {duration}"
            >> setField "testName" n
            >> setField "duration" d
            >> addExn e)

        summary = fun summary ->
          let spirit =
            if summary.errored.Length + summary.failed.Length = 0 then
              if Console.OutputEncoding.BodyName = "utf-8" then
                "ᕙ໒( ˵ ಠ ╭͜ʖ╮ ಠೃ ˵ )७ᕗ"
              else
                "Success!"
            else
              if Console.OutputEncoding.BodyName = "utf-8" then
                "( ರ Ĺ̯ ರೃ )"
              else
                ""
          logger.logWithAck Info (
            eventX "EXPECTO! {total} tests run in {duration} – {passes} passed, {ignores} ignored, {failures} failed, {errors} errored. {spirit}"
            >> setField "total" summary.total
            >> setField "duration" summary.duration
            >> setField "passes" summary.passed.Length
            >> setField "ignores" summary.ignored.Length
            >> setField "failures" summary.failed.Length
            >> setField "errors" summary.errored.Length
            >> setField "spirit" spirit)
          }

    static member summaryPrinter =
      { TestPrinters.defaultPrinter with
          summary = fun summary ->
            TestPrinters.defaultPrinter.summary summary |> Async.bind (fun () ->
            logSummary summary) }

    static member summaryWithLocationPrinter =
      { TestPrinters.defaultPrinter with
          summary = fun summary ->
            TestPrinters.defaultPrinter.summary summary |> Async.bind (fun () ->
            logSummaryWithLocation summary) }
    static member teamCityPrinter innerPrinter =
      // https://confluence.jetbrains.com/display/TCD10/Build+Script+Interaction+with+TeamCity#BuildScriptInteractionwithTeamCity-Escapedvalues
      let escape (msg: string) =
        let replaced =
          msg.Replace("|", "||")
            .Replace("'", "|'")
            .Replace("\r", "|r")
            .Replace("\n", "|n")
            .Replace("]", "|]")
            .Replace("[", "|[")

        let reg = Text.RegularExpressions.Regex(@"[^\u0020-\u007F]")
        reg.Replace(replaced, fun m -> "|0x" + ((int m.Value.[0]).ToString("X4")))

      let tcLog msgName props =
        let tcMsg =
          props
          |> List.map (fun (k,v) -> sprintf "%s='%s'" k (escape v))
          |> String.concat " "
          |> sprintf "##teamcity[%s %s]" msgName

        Global.lockSem (fun _ -> Console.WriteLine tcMsg)

      { beforeRun = fun _tests -> async {
          do! innerPrinter.beforeRun _tests
          tcLog "testSuiteStarted" [
            "name", "ExpectoTestSuite" ] }

        beforeEach = fun n -> async {
          do! innerPrinter.beforeEach n
          tcLog "testStarted" [
            "flowId", n
            "name", n ] }

        passed = fun n d -> async {
          do! innerPrinter.passed n d
          tcLog "testFinished" [
            "flowId", n
            "name", n
            "duration", d.TotalMilliseconds |> int |> string ] }

        info = fun s ->
          innerPrinter.info s

        ignored = fun n m -> async {
          do! innerPrinter.ignored n m
          tcLog "testIgnored" [
            "flowId", n
            "name", n
            "message", m ] }

        failed = fun n m d -> async {
          do! innerPrinter.failed n m d
          tcLog "testFailed" [
            "flowId", n
            "name", n
            "message", m ]
          tcLog "testFinished" [
            "flowId", n
            "name", n
            "duration", d.TotalMilliseconds |> int |> string ] }

        exn = fun n e d -> async {
          do! innerPrinter.beforeEach n
          tcLog "testFailed" [
            "flowId", n
            "name", n
            "message", e.Message
            "details", e.StackTrace ]
          tcLog "testFinished" [
            "flowId", n
            "name", n
            "duration", d.TotalMilliseconds |> int |> string ] }

        summary = fun s -> async {
          do! innerPrinter.summary s
          tcLog "testSuiteFinished" [
            "name", "ExpectoTestSuite" ] } }

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
          | UnFocused _ -> Some "The test is skipped because other tests are Focused"
          | Enabled _-> None

        /// tests: FlatTest list -> WrappedFlatTest list
        static member WrapStates (tests:FlatTest list) =
          let applyFocusedWrapping = function
            | Focused -> Enabled Focused
            | a -> UnFocused a
          let existsFocusedTests = tests |> List.exists (fun t -> FocusState.isFocused t.state)
          let wrappingMethod = if existsFocusedTests then applyFocusedWrapping else Enabled
          tests |> List.map (fun t -> {name=t.name; test=t.test; state=wrappingMethod t.state; sequenced=t.sequenced })

    and WrappedFlatTest =
      { name      : string
        test      : TestCode
        state     : WrappedFocusedState
        sequenced : bool }

  // Runner options
  type ExpectoConfig =
    { /// Whether to run the tests in parallel. Defaults to
      /// true, because your code should not mutate global
      /// state by default.
      parallel : bool
      /// Number of parallel workers. Defaults to the number of
      /// logical processors.
      parallelWorkers : int
      /// Whether to make the test runner fail if focused tests exist.
      /// This can be used from CI servers to ensure no focused tests are
      /// commited and therefor all tests are run.
      failOnFocusedTests : bool
      /// An optional filter function. Useful if you only would
      /// like to run a subset of all the tests defined in your assembly.
      filter   : Test -> Test
      /// Allows the test printer to be parametised to your liking.
      printer : TestPrinters
      /// Verbosity level (default: Info)
      verbosity : LogLevel
      /// Optional function used for finding source code location of test
      /// Defaults to empty source code
      locate : TestCode -> SourceLocation
    }

  let execTestAsync locate test =
    async {
      let w = Stopwatch.StartNew()
      try
        match test.state.ShouldSkipEvaluation with
        | Some ignoredMessage ->
          return { name     = test.name
                   result   = Ignored ignoredMessage
                   location = locate test.test
                   duration = TimeSpan.Zero }
        | None ->
          match test.test with
          | Async test ->
            do! test
          | Sync test ->
            test()
          w.Stop()
          return { name     = test.name
                   location = locate test.test
                   result   = Passed
                   duration = w.Elapsed }
      with
        | :? AssertException as e ->
          w.Stop()
          let msg =
            (stackTraceToString e.StackTrace).Split('\n')
            |> Seq.filter (fun q -> q.Contains ",1): ")
            |> Enumerable.FirstOrDefault
            |> sprintf "\n%s\n%s\n" e.Message
          return { name     = test.name
                   location = locate test.test
                   result   = Failed msg
                   duration = w.Elapsed }
        | :? FailedException as e ->
          w.Stop()
          return { name     = test.name
                   location = locate test.test
                   result   = Failed ("\n"+e.Message)
                   duration = w.Elapsed }
        | :? IgnoreException as e ->
          w.Stop()
          return { name     = test.name
                   location = locate test.test
                   result   = Ignored e.Message
                   duration = w.Elapsed }
        | e ->
          w.Stop()
          return { name     = test.name
                   location = locate test.test
                   result   = TestResult.Error e
                   duration = w.Elapsed }
    }

  /// Runs a list of tests, with parameterized printers (progress indicators) and traversal.
  /// Returns list of results.
  let evalTestList config map =

    let beforeEach test =
      config.printer.beforeEach test.name

    let printOne result =
      match result.result with
      | Passed -> config.printer.passed result.name result.duration
      | Failed message -> config.printer.failed result.name message result.duration
      | Ignored message -> config.printer.ignored result.name message
      | Error e -> config.printer.exn result.name e result.duration

    let pipeline test =
      async {
        let! beforeAsync = beforeEach test |> Async.StartChild
        let! result = execTestAsync config.locate test
        do! beforeAsync
        do! printOne result
        return result
      }

    WrappedFocusedState.WrapStates >> map pipeline

  let evalSilentAsync test =
    Test.toTestCodeList test
    |> WrappedFocusedState.WrapStates
    |> List.map (execTestAsync (fun _ -> SourceLocation.empty))
    |> Async.Parallel
    |> Async.map List.ofArray

  /// Runs a tree of tests, with parameterized printers (progress indicators) and traversal.
  /// Returns list of results.
  let eval config map tests =
    Test.toTestCodeList tests
    |> if config.parallel then id else List.map (fun t -> {t with sequenced=true})
    |> evalTestList config map

  /// Evaluates tests.
  let evalPar config tests =

    let pmap fn ts =
      if not config.``parallel`` || config.parallelWorkers = 1 then
          List.map (fn >> Async.RunSynchronously) ts
      else
        let sequenced =
          List.filter (fun t -> t.sequenced) ts
          |> List.map fn

        let parallel =
          List.filter (fun t -> not t.sequenced) ts
          |> List.map fn

        let parallelResults =
          let noWorkers =
            if config.parallelWorkers < 0 then
              -config.parallelWorkers * Environment.ProcessorCount
            elif config.parallelWorkers = 0 then
              Int32.MaxValue
            else
              config.parallelWorkers

          if List.isEmpty parallel then
            []
          elif List.length parallel <= noWorkers then
            Async.Parallel parallel
            |> Async.RunSynchronously
            |> List.ofArray
          else
            Async.parallelLimit noWorkers parallel
            |> Async.RunSynchronously

        if List.isEmpty sequenced |> not && List.isEmpty parallel |> not then
          config.printer.info "Starting sequenced tests..."
          |> Async.RunSynchronously

        let sequencedResults =
          List.map Async.RunSynchronously sequenced

        List.append sequencedResults parallelResults

    eval config pmap tests

  /// Runs tests, returns error code
  let runEval config (tests: Test) =
    config.printer.beforeRun tests |> Async.RunSynchronously

    let w = Stopwatch.StartNew()
    let results = evalPar config tests
    w.Stop()
    let testSummary = { sumTestResults results with duration = w.Elapsed }
    config.printer.summary testSummary |> Async.RunSynchronously

    TestResultSummary.errorCode testSummary

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

  // TODO: make internal
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

  // If the test function we've found doesn't seem to be in the test assembly, it's
  // possible we're looking at an FsCheck 'testProperty' style check. In that case,
  // the function of interest (i.e., the one in the test assembly, and for which we
  // might be able to find corresponding source code) is referred to in a field
  // of the function object.
  let isFsharpFuncType t =
    let baseType =
      let rec findBase (t:Type) =
        if t.BaseType = null || t.BaseType = typeof<obj> then
          t
        else
          findBase t.BaseType
      findBase t
    baseType.IsGenericType && baseType.GetGenericTypeDefinition() = typedefof<FSharpFunc<unit, unit>>

  let getFuncTypeToUse (testFunc:unit->unit) (asm:Assembly) =
    let t = testFunc.GetType()
    if t.Assembly.FullName = asm.FullName then
      t
    else
      let nestedFunc =
        t.GetFields()
        |> Seq.tryFind (fun f -> isFsharpFuncType f.FieldType)
      match nestedFunc with
      | Some f -> f.GetValue(testFunc).GetType()
      | None -> t

  let getMethodName asm testCode =
    match testCode with
    | Sync test ->
      let t = getFuncTypeToUse test asm
      let m = t.GetMethods () |> Seq.find (fun m -> (m.Name = "Invoke") && (m.DeclaringType = t))
      (t.FullName, m.Name)
    | Async _ ->
      ("Unknown Async", "Unknown Async")

  // Ported from
  // https://github.com/adamchester/expecto-adapter/blob/885fc9fff0/src/Expecto.VisualStudio.TestAdapter/SourceLocation.fs
  let getSourceLocation (asm:Assembly) className methodName =
    let lineNumberIndicatingHiddenLine = 0xfeefee
    let getEcma335TypeName (clrTypeName:string) = clrTypeName.Replace("+", "/")

    let types =
      let readerParams = new ReaderParameters( ReadSymbols = true )
      let moduleDefinition = ModuleDefinition.ReadModule(asm.Location, readerParams)

      seq { for t in moduleDefinition.GetTypes() -> (t.FullName, t) }
      |> Map.ofSeq

    let getMethods typeName =
      match types.TryFind (getEcma335TypeName typeName) with
      | Some t -> Some (t.GetMethods())
      | _ -> None

    let optionFromObj = function
      | null -> None
      | x -> Some x

    let getFirstOrDefaultSequencePoint (m:MethodDefinition) =
      m.Body.Instructions
      |> Seq.tryFind (fun i -> (i.SequencePoint <> null && i.SequencePoint.StartLine <> lineNumberIndicatingHiddenLine))
      |> Option.map (fun i -> i.SequencePoint)

    match getMethods className with
    | None -> SourceLocation.empty
    | Some methods ->
      let candidateSequencePoints =
        methods
        |> Seq.where (fun m -> m.Name = methodName)
        |> Seq.choose getFirstOrDefaultSequencePoint
        |> Seq.sortBy (fun sp -> sp.StartLine)
        |> Seq.toList
      match candidateSequencePoints with
      | [] -> SourceLocation.empty
      | xs -> {sourcePath = xs.Head.Document.Url ; lineNumber = xs.Head.StartLine}

  //val apply : f:(TestCode * FocusState * SourceLocation -> TestCode * FocusState * SourceLocation) -> _arg1:Test -> Test
  let getLocation (asm:Assembly) code =
    let typeName, methodName = getMethodName asm code
    try
      getSourceLocation asm typeName methodName
    with :? IO.FileNotFoundException as ioe ->
      SourceLocation.empty

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

  /// When the failOnFocusedTests switch is activated this function that no
  /// focused tests exist.
  ///
  /// Returns true if the check passes, otherwise false.
  let passesFocusTestCheck tests =
    let isFocused : FlatTest -> _ = function t when t.state = Focused -> true | _ -> false
    let focused = Test.toTestCodeList tests |> List.filter isFocused
    if focused.Length = 0 then true
    else
      logger.log LogLevel.Error (
        eventX "It was requested that no focused tests exist, but yet there are {count} focused tests found."
        >> setField "count" focused.Length)
      |> Async.Start
      false

[<AutoOpen; Extension>]
module Tests =
  open Impl
  open Helpers
  open Argu
  open Expecto.Logging
  open Expecto.Logging.Message

  /// Fail this test
  let inline failtest msg = raise <| AssertException msg
  /// Fail this test
  let inline failtestf fmt = Printf.ksprintf (AssertException >> raise) fmt
  /// Fail this test
  let inline failtestNoStack msg = raise <| FailedException msg
  /// Fail this test
  let inline failtestNoStackf fmt = Printf.ksprintf (FailedException >> raise) fmt

  /// Skip this test
  let inline skiptest msg = raise <| IgnoreException msg
  /// Skip this test
  let inline skiptestf fmt = Printf.ksprintf (IgnoreException >> raise) fmt

  /// Builds a list/group of tests that will be ignored by Expecto if exists
  /// focused tests and none of the parents is focused
  let inline testList name tests = TestLabel(name, TestList (tests, Normal), Normal)

  /// Builds a list/group of tests that will make Expecto to ignore other unfocused tests
  let inline ftestList name tests = TestLabel(name, TestList (tests, Focused), Focused)
  /// Builds a list/group of tests that will be ignored by Expecto
  let inline ptestList name tests = TestLabel(name, TestList (tests, Pending), Pending)

  /// Builds a test case that will be ignored by Expecto if exists focused
  /// tests and none of the parents is focused
  let inline testCase name test = TestLabel(name, TestCase (Sync test,Normal), Normal)
  /// Builds an async test case
  let inline testCaseAsync name test = TestLabel(name, TestCase (Async test,Normal), Normal)
  /// Builds a test case that will make Expecto to ignore other unfocused tests
  let inline ftestCase name test = TestLabel(name, TestCase (Sync test, Focused), Focused)
  /// Builds an async test case that will make Expecto to ignore other unfocused tests
  let inline ftestCaseAsync name test = TestLabel(name, TestCase (Async test, Focused), Focused)
  /// Builds a test case that will be ignored by Expecto
  let inline ptestCase name test = TestLabel(name, TestCase (Sync test, Pending), Pending)
  /// Builds an async test case that will be ignored by Expecto
  let inline ptestCaseAsync name test = TestLabel(name, TestCase (Async test, Pending), Pending)
  /// Test case or list needs to run sequenced. Use for any benchmark code or
  /// for tests using `Expect.isFasterThan`
  let inline testSequenced test = Sequenced test

  /// Applies a function to a list of values to build test cases
  let inline testFixture setup =
    Seq.map (fun (name, partialTest) ->
      testCase name (setup partialTest))

  /// Applies a value to a list of partial tests
  let inline testParam param =
    Seq.map (fun (name, partialTest) ->
      testCase name (partialTest param))

  // TODO: docs
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

  // TODO: docs
  let inline test name =
    TestCaseBuilder (name, Normal)
  // TODO: docs
  let inline ftest name =
    TestCaseBuilder (name, Focused)
  // TODO: docs
  let inline ptest name =
    TestCaseBuilder (name, Pending)

  /// Runs the passed tests
  let run config tests =
    runEval config tests

  /// The default configuration for Expecto.
  let defaultConfig =
    { parallel  = true
      parallelWorkers = Environment.ProcessorCount
      filter    = id
      failOnFocusedTests = false
      printer   =
        if Environment.GetEnvironmentVariable "TEAMCITY_PROJECT_NAME" <> null then
          TestPrinters.teamCityPrinter TestPrinters.defaultPrinter
        else
          TestPrinters.defaultPrinter
      verbosity = LogLevel.Info
      locate = fun _ -> SourceLocation.empty }

  // TODO: docs
  type CLIArguments =
    | Sequenced
    | Parallel
    | Parallel_Workers of int
    | Fail_On_Focused_Tests
    | Debug
    | Filter of hiera:string
    | Filter_Test_List of substring:string
    | Filter_Test_Case of substring:string
    | Run of tests:string list
    | List_Tests
    | Summary
    | Version
    | Summary_Location

    interface IArgParserTemplate with
      member s.Usage =
        match s with
        | Sequenced -> "Doesn't run the tests in parallel."
        | Parallel -> "Runs all tests in parallel (default)."
        | Parallel_Workers _ -> "Number of parallel workers (defaults to the number of logical processors)."
        | Fail_On_Focused_Tests -> "This will make the test runner fail if focused tests exist."
        | Debug -> "Extra verbose printing. Useful to combine with --sequenced."
        | Filter _ -> "Filters the list of tests by a hierarchy that's slash (/) separated."
        | Filter_Test_List _ -> "Filters the list of test lists by a substring."
        | Filter_Test_Case _ -> "Filters the list of test cases by a substring."
        | Run _ -> "Runs only provided tests."
        | List_Tests -> "Doesn't run tests, but prints out list of tests instead."
        | Summary -> "Prints out summary after all tests are finished."
        | Version -> "Prints out version information."
        | Summary_Location -> "Prints out summary after all tests are finished including their source code location"

  // TODO: docs
  [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
  module ExpectoConfig =

    let printExpectoVersion() =
      let assembly = Assembly.GetExecutingAssembly()
      let fileInfoVersion = FileVersionInfo.GetVersionInfo assembly.Location
      logger.info
        (eventX "EXPECTO version {version}"
          >> setField "version" fileInfoVersion.ProductVersion)

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
        | Parallel_Workers n -> fun o -> { o with parallelWorkers = n }
        | Fail_On_Focused_Tests -> fun o -> { o with failOnFocusedTests = true }
        | Debug -> fun o -> { o with verbosity = LogLevel.Debug }
        | Filter hiera -> fun o -> {o with filter = Test.filter (fun s -> s.StartsWith hiera )}
        | Filter_Test_List name ->  fun o -> {o with filter = Test.filter (fun s -> s |> getTestList |> Array.exists(fun s -> s.Contains name )) }
        | Filter_Test_Case name ->  fun o -> {o with filter = Test.filter (fun s -> s |> getTastCase |> fun s -> s.Contains name )}
        | Run tests -> fun o -> {o with filter = Test.filter (fun s -> tests |> List.exists ((=) s) )}
        | List_Tests -> id
        | Summary -> fun o -> {o with printer = TestPrinters.summaryPrinter}
        | Version -> fun o -> printExpectoVersion(); o
        | Summary_Location -> fun o -> {o with printer = TestPrinters.summaryWithLocationPrinter}

      fun (args: string[]) ->
        let parsed =
          parser.Parse(
            args,
            ignoreMissing = true,
            ignoreUnrecognized = true,
            raiseOnUsage = false)
        let isList = parsed.Contains <@ List_Tests @>

        parsed.GetAllResults()
        |> Seq.fold (flip reduceKnown) baseConfig, isList

  /// Prints out names of all tests for given test suite.
  let listTests test =
    test
    |> Test.toTestCodeList
    |> Seq.iter (fun t -> printfn "%s" t.name)


  /// Runs tests with the supplied options.
  /// Returns 0 if all tests passed, otherwise 1
  let runTests config (tests:Test) =
    Global.initialiseIfDefault
      { Global.defaultConfig with
          getLogger = fun name -> LiterateConsoleTarget(name, config.verbosity, consoleSemaphore = Global.semaphore()) :> Logger }
    if not config.failOnFocusedTests || passesFocusTestCheck tests then
      run config tests
    else
      1

  /// Runs all given tests with the supplied command-line options.
  /// Returns 0 if all tests passed, otherwise 1
  let runTestsWithArgs config args tests =
    let config, isList = ExpectoConfig.fillFromArgs config args
    let tests = config.filter tests
    if isList then
      listTests tests
      0
    else
      runTests config tests

  /// Runs tests in this assembly with the supplied command-line options.
  /// Returns 0 if all tests passed, otherwise 1
  let runTestsInAssembly config args =
    let config = { config with locate = getLocation (Assembly.GetEntryAssembly()) }
    testFromThisAssembly ()
    |> Option.orDefault (TestList ([], Normal))
    |> runTestsWithArgs config args

// TODO: docs
type Accuracy =
  { absolute: float
    relative: float }

// TODO: docs
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Accuracy =
  let inline areCloseLhs a b = abs(a-b)
  let inline areCloseRhs m a b = m.absolute + m.relative * max (abs a) (abs b)
  let inline areClose m a b = areCloseLhs a b <= areCloseRhs m a b

  let low =
    {absolute=1e-6; relative=1e-3}
  let medium =
    {absolute=1e-8; relative=1e-5}
  let high =
    {absolute=1e-10; relative=1e-7}
  let veryHigh =
    {absolute=1e-12; relative=1e-9}