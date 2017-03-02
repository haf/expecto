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

type FsCheckConfig =
  { maxTest: int
    startSize: int
    endSize: int
    replay: (int*int) option
    arbitrary: Type list }
  static member defaultConfig =
    { maxTest = 100
      startSize = 1
      endSize = 100
      replay = None
      arbitrary = [] }

/// Actual test function; either an async one, or a synchronous one.
type TestCode =
  | Sync of stest: (unit -> unit)
  | Async of atest: Async<unit>
  | AsyncFsCheck of testConfig: FsCheckConfig option *
                    stressConfig: FsCheckConfig option *
                    test: (FsCheckConfig -> Async<unit>)

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


type SequenceMethod =
  | Synchronous
  | SynchronousGroup of string
  | InParallel

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
  | Sequenced of SequenceMethod * Test

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


[<AutoOpen>]
module internal Helpers =
  let inline dispose (d:IDisposable) = d.Dispose()
  let inline addFst a b = a,b
  let inline addSnd b a = a,b
  let inline commaString (i:int) = i.ToString("#,##0")
  open System.Text.RegularExpressions
  let rx = lazy Regex(" at (.*) in (.*):line (\d+)", RegexOptions.Compiled ||| RegexOptions.Multiline)
  let stackTraceToString s = rx.Value.Replace(s, "$2($3,1): $1")
  let exnToString (e: Exception) = stackTraceToString (e.ToString())

  module Seq =
    let cons x xs = seq { yield x; yield! xs }

  module List =
    let inline singleton x = [x]

  module Option =
    let orFun fn =
      function | Some a -> a | None -> fn()
    let orDefault def =
      function | Some a -> a | None -> def

  type Type with
    static member TryGetType t =
      try
        Type.GetType(t, true) |> Some
      with _ ->
        None

  type ResizeMap<'k,'v> = Collections.Generic.Dictionary<'k,'v>

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


module internal Async =
  let map fn a =
    async {
      let! v = a
      return fn v
    }

  let bind fn a =
    async.Bind(a, fn)

  let foldSequentiallyWithCancel (ct:CancellationTokenSource)
                                 folder state (s:_ seq) =
    let mutable state = state
    Async.Start(async {
      use e = s.GetEnumerator()
      while not ct.IsCancellationRequested && e.MoveNext() do
        let! item = e.Current
        state <- folder state item
      ct.Cancel()
    }, ct.Token)
    Async.AwaitWaitHandle ct.Token.WaitHandle |> map (fun _ -> state)

  let foldSequentially folder state (s:_ seq) =
    foldSequentiallyWithCancel (new CancellationTokenSource())
                               folder state s

  let foldParallelLimitWithCancel (ct:CancellationTokenSource)
                                  maxParallelism folder state (s:_ seq) =
    let mutable state = state
    let enumerator = s.GetEnumerator()
    use mb =
      MailboxProcessor.Start((fun mb ->
        let rec loop running =
          async {
            // by receiving one by one, we ensure we can mutate `results`
            let! msg = mb.Receive()
            match msg with
            // None acts as a kanban card – running another one
            | None ->
              if enumerator.MoveNext() then
                let next = enumerator.Current
                Async.Start(async {
                  try
                    let! r = next
                    Some r |> mb.Post
                  finally
                    mb.Post None
                }, ct.Token)
                return! loop (running+1)
              elif running = 0 then
                ct.Cancel()
              else
                return! loop running
            // the other side of the coin; receiving results
            | Some r ->
              state <- folder state r
              return! loop (running-1)
          }
        loop 0), ct.Token)

    let rec start n =
      if n > 0 then
        mb.Post None
        start (n-1)

    start maxParallelism
    Async.AwaitWaitHandle ct.Token.WaitHandle
    |> map (fun _ -> state)

  let foldParallelLimit maxParallelism folder state (s:_ seq) =
    foldParallelLimitWithCancel (new CancellationTokenSource())
                                maxParallelism folder state s

// TODO: move to internal
[<ReferenceEquality>]
type FlatTest =
  { name      : string
    test      : TestCode
    state     : FocusState
    focusOn   : bool
    sequenced : SequenceMethod }
  member x.shouldSkipEvaluation =
    match x.focusOn, x.state with
    | _, Pending -> Some "The test or one of his parents is marked as Pending"
    | true, Normal -> Some "The test is skipped because other tests are Focused"
    | _ -> None

[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module Test =
  /// Compute the child test state based on parent test state
  let computeChildFocusState parentState childState =
    match parentState, childState with
    | Focused, Pending -> Pending
    | Pending, _ -> Pending
    | Focused, _ -> Focused
    | Normal, _ -> childState

  /// Is focused set on at least one test
  let rec isFocused test =
    match test with
    | TestLabel (_,_,Focused)
    | TestCase (_,Focused)
    | TestList (_,Focused) -> true
    | TestLabel (_,_,Pending)
    | TestList (_,Pending)
    | TestCase _ -> false
    | TestLabel (_,test,Normal)
    | Sequenced (_,test) -> isFocused test
    | TestList (tests,Normal) -> List.exists isFocused tests

  /// Flattens a tree of tests
  let toTestCodeList test =
    let isFocused = isFocused test
    let rec loop parentName testList parentState sequenced =
      function
      | TestLabel (name, test, state) ->
        let fullName =
          if String.IsNullOrEmpty parentName
            then name
            else parentName + "/" + name
        loop fullName testList (computeChildFocusState parentState state) sequenced test
      | TestCase (test, state) ->
        { name=parentName
          test=test
          state=computeChildFocusState parentState state
          focusOn = isFocused
          sequenced=sequenced } :: testList
      | TestList (tests, state) -> List.collect (loop parentName testList (computeChildFocusState parentState state) sequenced) tests
      | Sequenced (sequenced,test) -> loop parentName testList parentState sequenced test
    loop null [] Normal InParallel test

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
    | Sequenced (sequenced,test) -> Sequenced (sequenced,translateFocusState newFocusState test)

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
    | Sequenced (sequenced,test) -> Sequenced (sequenced,replaceTestCode f test)

  /// Filter tests by name.
  let filter pred =
    toTestCodeList
    >> List.filter (fun t -> pred t.name)
    >> List.map (fun t ->
        let test = TestLabel (t.name, TestCase (t.test, t.state), t.state)
        match t.sequenced with
        | InParallel ->
          test
        | s ->
          Sequenced (s,test)
      )
    >> (fun x -> TestList (x,Normal))

  /// Applies a timeout to a test.
  let timeout timeout (test: TestCode) : TestCode =
    let timeoutAsync testAsync =
      async {
        try
          let! async = Async.StartChild(testAsync, timeout)
          do! async
        with :? TimeoutException ->
          let ts = TimeSpan.FromMilliseconds (float timeout)
          raise <| AssertException(sprintf "Timeout (%A)" ts)
      }

    match test with
    | Sync test -> async { test() } |> timeoutAsync |> Async
    | Async test -> timeoutAsync test |> Async
    | AsyncFsCheck (testConfig, stressConfig, test) ->
      AsyncFsCheck (testConfig, stressConfig, test >> timeoutAsync)



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
    member x.tag =
      match x with
      | Passed -> 0
      | Ignored _ -> 1
      | Failed _ -> 2
      | Error _ -> 3
    member x.isPassed =
      match x with
      | Passed -> true
      | _ -> false
    member x.isIgnored =
      match x with
      | Ignored _ -> true
      | _ -> false
    member x.isFailed =
      match x with
      | Failed _ -> true
      | _ -> false
    member x.isException =
      match x with
      | Error _ -> true
      | _ -> false
    static member max (a:TestResult) (b:TestResult) =
      if a.tag>=b.tag then a else b

  type TestSummary =
    { result        : TestResult
      count         : int
      meanDuration  : float
      maxDuration   : float
      totalVariance : float }
    member x.duration = TimeSpan.FromMilliseconds x.meanDuration
    static member single result duration =
      { result        = result
        count         = 1
        meanDuration  = duration
        maxDuration   = duration
        totalVariance = 0.0 }
    static member (+)(s:TestSummary,(r,d):TestResult*float) =
      let n,m,v = Statistics.updateIntermediateStatistics
                    (s.count,s.meanDuration,s.totalVariance) d
      { result        = TestResult.max s.result r
        count         = n
        meanDuration  = m
        maxDuration   = max s.maxDuration d
        totalVariance = v }

  type TestRunSummary =
    { results   : (FlatTest*TestSummary) list
      duration  : TimeSpan
      maxMemory : int64
      memoryLimit : int64
      timedOut  : FlatTest list
    }
    static member fromResults results =
      { results  = results
        duration =
            results
            |> List.sumBy (fun (_,r:TestSummary) -> r.meanDuration)
            |> TimeSpan.FromMilliseconds
        maxMemory = 0L
        memoryLimit = 0L
        timedOut = [] }
    member x.passed = List.filter (fun (_,r) -> r.result.isPassed) x.results
    member x.ignored = List.filter (fun (_,r) -> r.result.isIgnored) x.results
    member x.failed = List.filter (fun (_,r) -> r.result.isFailed) x.results
    member x.errored = List.filter (fun (_,r) -> r.result.isException) x.results
    member x.errorCode =
      (if List.isEmpty x.failed then 0 else 1) |||
      (if List.isEmpty x.errored then 0 else 2) |||
      (if x.maxMemory <= x.memoryLimit then 0 else 4) |||
      (if List.isEmpty x.timedOut then 0 else 8)
    member x.successful = x.errorCode = 0

  let createSummaryMessage (summary: TestRunSummary) =
    let handleLineBreaks (elements:(FlatTest*TestSummary) seq) =
        elements
        |> Seq.map (fun (n,_) -> "\n\t" + n.name)
        |> String.Concat

    let passed = summary.passed |> handleLineBreaks
    let passedCount = List.sumBy (fun (_,r) -> r.count) summary.passed |> commaString
    let ignored = summary.ignored |> handleLineBreaks
    let ignoredCount = List.sumBy (fun (_,r) -> r.count) summary.ignored |> commaString
    let failed = summary.failed |> handleLineBreaks
    let failedCount = List.sumBy (fun (_,r) -> r.count) summary.failed |> commaString
    let errored = summary.errored |> handleLineBreaks
    let erroredCount = List.sumBy (fun (_,r) -> r.count) summary.errored |> commaString

    let digits =
      [passedCount; ignoredCount; failedCount; erroredCount ]
      |> List.map (fun x -> x.ToString().Length)
      |> List.max

    let align (s:string) offset = s.PadLeft(offset + digits)

    eventX "EXPECTO?! Summary...\nPassed: {passedCount}{passed}\nIgnored: {ignoredCount}{ignored}\nFailed: {failedCount}{failed}\nErrored: {erroredCount}{errored}"
    >> setField "passed" passed
    >> setField "passedCount" (align passedCount 1)
    >> setField "ignored" ignored
    >> setField "ignoredCount" (align ignoredCount 0)
    >> setField "failed" failed
    >> setField "failedCount" (align failedCount 1)
    >> setField "errored" errored
    >> setField "erroredCount" (align erroredCount 0)

  let createSummaryText (summary: TestRunSummary) =
    createSummaryMessage summary Info
    |> Expecto.Logging.Formatting.defaultFormatter

  let logSummary (summary: TestRunSummary) =
    createSummaryMessage summary
    |> logger.logWithAck Info

  let logSummaryWithLocation locate (summary: TestRunSummary) =
    let handleLineBreaks (elements:(FlatTest*TestSummary) seq) =
      let format (n:FlatTest,_) =
        let location = locate n.test
        sprintf "%s [%s:%d]" n.name location.sourcePath location.lineNumber

      let text = elements |> Seq.map format |> String.concat "\n\t"
      if text = "" then text else text + "\n"

    let passed = summary.passed |> handleLineBreaks
    let passedCount = List.sumBy (fun (_,r) -> r.count) summary.passed |> commaString
    let ignored = summary.ignored |> handleLineBreaks
    let ignoredCount = List.sumBy (fun (_,r) -> r.count) summary.ignored |> commaString
    let failed = summary.failed |> handleLineBreaks
    let failedCount = List.sumBy (fun (_,r) -> r.count) summary.failed |> commaString
    let errored = summary.errored |> handleLineBreaks
    let erroredCount = List.sumBy (fun (_,r) -> r.count) summary.errored |> commaString

    let digits =
      [passedCount; ignoredCount; failedCount; erroredCount ]
      |> List.map (fun x -> x.ToString().Length)
      |> List.max

    let align (s:string) offset = s.PadLeft(offset + digits)

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
      summary : ExpectoConfig -> TestRunSummary -> Async<unit> }

    static member printResult config (test:FlatTest) (result:TestSummary) =
      match result.result with
      | Passed -> config.printer.passed test.name result.duration
      | Failed message -> config.printer.failed test.name message result.duration
      | Ignored message -> config.printer.ignored test.name message
      | Error e -> config.printer.exn test.name e result.duration

    static member silent =
      { beforeRun = fun _ -> async.Zero()
        beforeEach = fun _ -> async.Zero()
        info = fun _ -> async.Zero()
        passed = fun _ _ -> async.Zero()
        ignored = fun _ _ -> async.Zero()
        failed = fun _ _ _ -> async.Zero()
        exn = fun _ _ _ -> async.Zero()
        summary = fun _ _ -> async.Zero() }

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

        summary = fun _ summary ->
          let spirit =
            if summary.successful then
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
            >> setField "total" (List.sumBy (fun (_,r) -> r.count) summary.results |> commaString)
            >> setField "duration" summary.duration
            >> setField "passes" (List.sumBy (fun (_,r) -> r.count) summary.passed |> commaString)
            >> setField "ignores" (List.sumBy (fun (_,r) -> r.count) summary.ignored |> commaString)
            >> setField "failures" (List.sumBy (fun (_,r) -> r.count) summary.failed |> commaString)
            >> setField "errors" (List.sumBy (fun (_,r) -> r.count) summary.errored |> commaString)
            >> setField "spirit" spirit)
          }

    static member stressPrinter =
      { TestPrinters.defaultPrinter with
          beforeRun = fun _tests ->
            logger.logWithAck Info (
              eventX "EXPECTO? Running stress testing...")
          summary = fun config summary ->
            let printResults =
              List.map (fun (t,r) -> TestPrinters.printResult config t r) summary.results
              |> Async.foldSequentially (fun _ _ -> ()) ()
            let result =
              if summary.maxMemory > summary.memoryLimit then
                logger.logWithAck LogLevel.Error (
                  eventX "Maximum memory usage was {memory} KB and exceeded the limit set at {limit} KB.\nRunning tests:\n\t{timeout}"
                  >> setField "memory" (summary.maxMemory / 1024L |> int |> commaString)
                  >> setField "limit" (summary.memoryLimit / 1024L |> int |> commaString)
                  >> setField "timeout" (summary.timedOut |> Seq.map (fun t -> t.name) |> String.concat "\n\t"))
              elif List.isEmpty summary.timedOut then
                logger.logWithAck Info (
                  eventX "Maximum memory usage was {memory} KB (limit set at {limit} KB)."
                  >> setField "memory" (summary.maxMemory / 1024L |> int |> commaString)
                  >> setField "limit" (summary.memoryLimit / 1024L |> int |> commaString))
              else
                logger.logWithAck LogLevel.Error (
                  eventX "Deadlock timeout running tests:\n\t{timeout}"
                  >> setField "timeout" (summary.timedOut |> Seq.map (fun t -> t.name) |> String.concat "\n\t"))
            async {
              do! printResults
              do! result
              do! TestPrinters.defaultPrinter.summary config summary
            }
          }

    static member summaryPrinter =
      { TestPrinters.defaultPrinter with
          summary = fun config summary ->
            TestPrinters.defaultPrinter.summary config summary
            |> Async.bind (fun () -> logSummary summary) }

    static member summaryWithLocationPrinter =
      { TestPrinters.defaultPrinter with
          summary = fun config summary ->
            TestPrinters.defaultPrinter.summary config summary
            |> Async.bind (fun () -> logSummaryWithLocation config.locate summary) }
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

        summary = fun c s -> async {
          do! innerPrinter.summary c s
          tcLog "testSuiteFinished" [
            "name", "ExpectoTestSuite" ] } }

  // Runner options
  and ExpectoConfig =
    { /// Whether to run the tests in parallel. Defaults to
      /// true, because your code should not mutate global
      /// state by default.
      parallel : bool
      /// Number of parallel workers. Defaults to the number of
      /// logical processors.
      parallelWorkers : int
      /// Stress test by running tests randomly for the given TimeSpan.
      /// Can be sequenced or parallel depending on the config.
      stress : TimeSpan option
      /// Stress test deadlock timeout TimeSpan to wait after stress TimeSpan
      /// before stopping and reporting as a deadlock (default 5 mins).
      stressTimeout : TimeSpan
      /// Stress test memory limit in MB to stop the test and report as
      /// a memory leak (default 100 MB)
      stressMemoryLimit : float
      /// Whether to make the test runner fail if focused tests exist.
      /// This can be used from CI servers to ensure no focused tests are
      /// commited and therefor all tests are run.
      failOnFocusedTests : bool
      /// An optional filter function. Useful if you only would
      /// like to run a subset of all the tests defined in your assembly.
      filter   : Test -> Test
      /// Allows the test printer to be parametised to your liking.
      printer : TestPrinters
      /// Verbosity level (default: Info).
      verbosity : LogLevel
      /// Optional function used for finding source code location of test
      /// Defaults to empty source code.
      locate : TestCode -> SourceLocation
      /// FsCheck maximum number of tests (default: 100).
      fsCheckMaxTests: int
      /// FsCheck start size (default: 1).
      fsCheckStartSize: int
      /// FsCheck end size (default: 100 for testing and 10,000 for
      /// stress testing).
      fsCheckEndSize: int option
    }
    static member defaultConfig =
      { parallel = true
        parallelWorkers = Environment.ProcessorCount
        stress = None
        stressTimeout = TimeSpan.FromMinutes 5.0
        stressMemoryLimit = 100.0
        filter = id
        failOnFocusedTests = false
        printer =
          if Environment.GetEnvironmentVariable "TEAMCITY_PROJECT_NAME" <> null then
            TestPrinters.teamCityPrinter TestPrinters.defaultPrinter
          else
            TestPrinters.defaultPrinter
        verbosity = LogLevel.Info
        locate = fun _ -> SourceLocation.empty
        fsCheckMaxTests = 100
        fsCheckStartSize = 1
        fsCheckEndSize = None
      }

  let execTestAsync config (test:FlatTest) : Async<TestSummary> =
    async {
      let w = Stopwatch.StartNew()
      try
        match test.shouldSkipEvaluation with
        | Some ignoredMessage ->
          return TestSummary.single (Ignored ignoredMessage) 0.0
        | None ->
          match test.test with
          | Sync test ->
            test()
          | Async test ->
            do! test
          | AsyncFsCheck (testConfig, stressConfig, test) ->
            let fsConfig =
              match config.stress with
              | None -> testConfig
              | Some _ -> stressConfig
              |> Option.orFun (fun () ->
                  { FsCheckConfig.defaultConfig with
                      maxTest = config.fsCheckMaxTests
                      startSize = config.fsCheckStartSize
                      endSize =
                        match config.fsCheckEndSize, config.stress with
                        | Some i, _ -> i
                        | None, None -> 100
                        | None, Some _ -> 10000
                  }
                )
            do! test fsConfig
          w.Stop()
          return TestSummary.single Passed (float w.ElapsedMilliseconds)
      with
        | :? AssertException as e ->
          w.Stop()
          let msg =
            (stackTraceToString e.StackTrace).Split('\n')
            |> Seq.filter (fun q -> q.Contains ",1): ")
            |> Enumerable.FirstOrDefault
            |> sprintf "\n%s\n%s\n" e.Message
          return
            TestSummary.single (Failed msg) (float w.ElapsedMilliseconds)
        | :? FailedException as e ->
          w.Stop()
          return
            TestSummary.single (Failed ("\n"+e.Message)) (float w.ElapsedMilliseconds)
        | :? IgnoreException as e ->
          w.Stop()
          return
            TestSummary.single (Ignored e.Message) (float w.ElapsedMilliseconds)
        | e ->
          w.Stop()
          return
            TestSummary.single (TestResult.Error e) (float w.ElapsedMilliseconds)
    }

  let private numberOfWorkers limit config =
    if config.parallelWorkers < 0 then
      -config.parallelWorkers * Environment.ProcessorCount
    elif config.parallelWorkers = 0 then
      if limit then
        Environment.ProcessorCount
      else
        Int32.MaxValue
    else
      config.parallelWorkers

  /// Evaluates tests.
  let evalTests config test =
    async {

      let evalTestAsync (test:FlatTest) =

        let beforeEach (test:FlatTest) =
          config.printer.beforeEach test.name

        async {
          let! beforeAsync = beforeEach test |> Async.StartChild
          let! result = execTestAsync config test
          do! beforeAsync
          do! TestPrinters.printResult config test result
          return test,result
        }

      let tests = Test.toTestCodeList test
      let inline cons xs x = x::xs

      if not config.``parallel`` ||
         config.parallelWorkers = 1 ||
         List.forall (fun t -> t.sequenced=Synchronous) tests then
        return!
          List.map evalTestAsync tests
          |> Async.foldSequentially cons []
      else
        let sequenced =
          List.filter (fun t -> t.sequenced=Synchronous) tests
          |> List.map evalTestAsync

        let parallel =
          List.filter (fun t -> t.sequenced<>Synchronous) tests
          |> Seq.groupBy (fun t -> t.sequenced)
          |> Seq.collect(fun (group,tests) ->
              match group with
              | InParallel ->
                Seq.map (evalTestAsync >> List.singleton) tests
              | _ ->
                Seq.map evalTestAsync tests
                |> Seq.toList
                |> Seq.singleton
            )
          |> Seq.toList
          |> List.sortBy (List.length >> (~-))
          |> List.map (
              function
              | [test] ->
                Async.map List.singleton test
              | l ->
                Async.foldSequentially cons [] l
            )

        let! parallelResults =
          let noWorkers = numberOfWorkers false config
          if List.length parallel <= noWorkers then
            Async.Parallel parallel
            |> Async.map (Seq.concat >> Seq.toList)
          else
            Async.foldParallelLimit noWorkers (@) [] parallel

        if List.isEmpty sequenced |> not && List.isEmpty parallel |> not then
          do! config.printer.info "Starting sequenced tests..."

        return! Async.foldSequentially cons parallelResults sequenced
      }

  let evalTestsSilent test =
    let config =
      { ExpectoConfig.defaultConfig with
          parallel = false
          verbosity = LogLevel.Fatal
          printer = TestPrinters.silent
      }
    evalTests config test

  /// Runs tests, returns error code
  let runEval config test =
    async {
      do! config.printer.beforeRun test

      let w = Stopwatch.StartNew()
      let! results = evalTests config test
      w.Stop()
      let testSummary = { results = results
                          duration = w.Elapsed
                          maxMemory = 0L
                          memoryLimit = 0L
                          timedOut = [] }
      do! config.printer.summary config testSummary

      return testSummary.errorCode
    }

  let runStress config test =
    async {
      do! config.printer.beforeRun test

      let tests =
        Test.toTestCodeList test
        |> List.filter (fun t -> Option.isNone t.shouldSkipEvaluation)

      let memoryLimit =
        config.stressMemoryLimit * 1024.0 * 1024.0 |> int64

      let evalTestAsync test =
        execTestAsync config test |> Async.map (addFst test)

      let rand = Random()

      let randNext tests =
        List.length tests |> rand.Next |> List.nth tests

      let finishTimestamp =
        lazy
        config.stress.Value.TotalSeconds * float Stopwatch.Frequency
        |> int64
        |> (+) (Stopwatch.GetTimestamp())

      let asyncRun foldRunner (runningTests:ResizeArray<_>,
                               results,
                               maxMemory) =
        let cancel = new CancellationTokenSource()

        let folder (runningTests:ResizeArray<_>,
                    results:ResizeMap<_,_>,
                    maxMemory) (test,result) =

          runningTests.Remove test |> ignore

          results.[test] <-
            match results.TryGetValue test with
            | true, existing ->
              existing + (result.result,result.meanDuration)
            | false, _ ->
              result

          let maxMemory = GC.GetTotalMemory false |> max maxMemory

          if maxMemory > memoryLimit then
            cancel.Cancel()

          runningTests, results, maxMemory

        Async.Start(async {
          let finishMilliseconds =
            max (finishTimestamp.Value - Stopwatch.GetTimestamp()) 0L
            * 1000L / Stopwatch.Frequency
          let timeout =
            int finishMilliseconds + int config.stressTimeout.TotalMilliseconds
          do! Async.Sleep timeout
          cancel.Cancel()
        }, cancel.Token)

        Seq.takeWhile (fun test ->
          if Stopwatch.GetTimestamp() < finishTimestamp.Value then
            runningTests.Add test
            true
          else
            false )
        >> Seq.map evalTestAsync
        >> foldRunner cancel folder (runningTests,results,maxMemory)

      let initial = ResizeArray(), ResizeMap(), GC.GetTotalMemory false

      let w = Stopwatch.StartNew()

      let! runningTests,results,maxMemory =
        if not config.``parallel`` ||
           config.parallelWorkers = 1 ||
           List.forall (fun t -> t.sequenced=Synchronous) tests then

          Seq.initInfinite (fun _ -> randNext tests)
          |> Seq.append tests
          |> asyncRun Async.foldSequentiallyWithCancel initial
        else
          List.filter (fun t -> t.sequenced=Synchronous) tests
          |> asyncRun Async.foldSequentiallyWithCancel initial
          |> Async.bind (fun (runningTests,results,maxMemory) ->
               if maxMemory > memoryLimit ||
                  Stopwatch.GetTimestamp() > finishTimestamp.Value then
                 async.Return (runningTests,results,maxMemory)
               else
                 let parallel =
                   List.filter (fun t -> t.sequenced<>Synchronous) tests
                 Seq.initInfinite (fun _ -> randNext parallel)
                 |> Seq.append parallel
                 |> Seq.filter (fun test ->
                      let s = test.sequenced
                      s=InParallel ||
                      not(Seq.exists (fun t -> t.sequenced=s) runningTests)
                    )
                 |> asyncRun (fun cancel ->
                      Async.foldParallelLimitWithCancel cancel
                                (numberOfWorkers true config)
                    ) (runningTests,results,maxMemory)
             )

      w.Stop()

      let testSummary = { results = results
                                    |> Seq.map (fun kv -> kv.Key,kv.Value)
                                    |> List.ofSeq
                          duration = w.Elapsed
                          maxMemory = maxMemory
                          memoryLimit = memoryLimit
                          timedOut = List.ofSeq runningTests }

      do! config.printer.summary config testSummary

      return testSummary.errorCode
    }

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
    | Async _ | AsyncFsCheck _ ->
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
  let passesFocusTestCheck config tests =
    let isFocused : FlatTest -> _ = function t when t.state = Focused -> true | _ -> false
    let focused = Test.toTestCodeList tests |> List.filter isFocused
    if focused.Length = 0 then true
    else
      if config.verbosity <> LogLevel.Fatal then
        logger.log LogLevel.Error (
          eventX "It was requested that no focused tests exist, but yet there are {count} focused tests found."
          >> setField "count" focused.Length)
        |> Async.StartImmediate
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
  let inline testSequenced test = Sequenced (Synchronous,test)
  /// Test case or list needs to run sequenced with other tests in this group.
  let inline testSequencedGroup name test = Sequenced (SynchronousGroup name,test)

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
    member __.TryFinally(f, compensation) =
      try
        f()
      finally
        compensation()
    member __.TryWith(f, catchHandler) =
      try
        f()
      with e -> catchHandler e
    member __.Using(disposable: #IDisposable, f) =
      try
        f disposable
      finally
        match disposable with
        | null -> ()
        | disp -> disp.Dispose()
    member __.For(sequence, f) =
      for i in sequence do f i
    member __.Combine(f1, f2) = f2(); f1
    member __.Zero() = ()
    member __.Delay f = f
    member __.Run f =
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

  /// The default configuration for Expecto.
  let defaultConfig = ExpectoConfig.defaultConfig

  /// The CLI arguments are the parameters that are possible to send to Expecto
  /// and change the runner's behaviour.
  type CLIArguments =
    | Sequenced
    | Parallel
    | Parallel_Workers of int
    | FsCheck_Max_Tests of int
    | FsCheck_Start_Size of int
    | FsCheck_End_Size of int
    | Stress of float
    | Stress_Timeout of float
    | Stress_Memory_Limit of float
    | Fail_On_Focused_Tests
    | Debug
    | Filter of hiera:string
    | Filter_Test_List of substring:string
    | Filter_Test_Case of substring:string
    | Run of tests:string list
    | List_Tests
    | Summary
    | Summary_Location
    | Version

    interface IArgParserTemplate with
      member s.Usage =
        match s with
        | Sequenced -> "doesn't run the tests in parallel."
        | Parallel -> "runs all tests in parallel (default)."
        | Parallel_Workers _ -> "number of parallel workers (defaults to the number of logical processors)."
        | Stress _ -> "run the tests randomly for the given number of minutes."
        | Stress_Timeout _ -> "time to wait in minutes after the stress test before reporting as a deadlock (default 5 mins)."
        | Stress_Memory_Limit _ -> "stress test memory limit in MB to stop the test and report as a memory leak (default 100 MB)."
        | Fail_On_Focused_Tests -> "this will make the test runner fail if focused tests exist."
        | Debug -> "extra verbose printing. Useful to combine with --sequenced."
        | Filter _ -> "filters the list of tests by a hierarchy that's slash (/) separated."
        | Filter_Test_List _ -> "filters the list of test lists by a substring."
        | Filter_Test_Case _ -> "filters the list of test cases by a substring."
        | Run _ -> "runs only provided tests."
        | List_Tests -> "doesn't run tests, but prints out list of tests instead."
        | Summary -> "prints out summary after all tests are finished."
        | Version -> "prints out version information."
        | Summary_Location -> "prints out summary after all tests are finished including their source code location."
        | FsCheck_Max_Tests _ -> "FsCheck maximum number of tests (default: 100)."
        | FsCheck_Start_Size _ -> "FsCheck start size (default: 1)."
        | FsCheck_End_Size _ -> "FsCheck end size (default: 100 for testing and 10,000 for stress testing)."

  type FillFromArgsResult =
    | ArgsRun of ExpectoConfig
    | ArgsList of ExpectoConfig
    | ArgsVersion of ExpectoConfig
    | ArgsUsage of usage:string * optionErrors:string list
    | ArgsException of usage:string * exceptionMessage:string

  // TODO: docs
  [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
  module ExpectoConfig =

    let expectoVersion() =
      let assembly = Assembly.GetExecutingAssembly()
      let fileInfoVersion = FileVersionInfo.GetVersionInfo assembly.Location
      fileInfoVersion.ProductVersion

    /// Parses command-line arguments into a config. This allows you to
    /// override the config from the command line, rather than having
    /// to go into the compiled code to change how they are being run.
    /// Also checks if tests should be run or only listed
    let fillFromArgs baseConfig args =
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
        | Stress n -> fun o  -> {o with
                                    stress = TimeSpan.FromMinutes n |> Some
                                    printer = TestPrinters.stressPrinter }
        | Stress_Timeout n -> fun o -> { o with stressTimeout = TimeSpan.FromMinutes n }
        | Stress_Memory_Limit n -> fun o -> { o with stressMemoryLimit = n }
        | Fail_On_Focused_Tests -> fun o -> { o with failOnFocusedTests = true }
        | Debug -> fun o -> { o with verbosity = LogLevel.Debug }
        | Filter hiera -> fun o -> {o with filter = Test.filter (fun s -> s.StartsWith hiera )}
        | Filter_Test_List name ->  fun o -> {o with filter = Test.filter (fun s -> s |> getTestList |> Array.exists(fun s -> s.Contains name )) }
        | Filter_Test_Case name ->  fun o -> {o with filter = Test.filter (fun s -> s |> getTastCase |> fun s -> s.Contains name )}
        | Run tests -> fun o -> {o with filter = Test.filter (fun s -> tests |> List.exists ((=) s) )}
        | List_Tests -> id
        | Summary -> fun o -> {o with printer = TestPrinters.summaryPrinter}
        | Version -> id
        | Summary_Location -> fun o -> {o with printer = TestPrinters.summaryWithLocationPrinter}
        | FsCheck_Max_Tests n -> fun o -> {o with fsCheckMaxTests = n }
        | FsCheck_Start_Size n -> fun o -> {o with fsCheckStartSize = n }
        | FsCheck_End_Size n -> fun o -> {o with fsCheckEndSize = Some n }

      let parsed =
        try
          parser.Parse(
            args,
            ignoreMissing = true,
            ignoreUnrecognized = true,
            raiseOnUsage = false)
          |> Choice1Of2
        with
        | e -> Choice2Of2 e.Message

      match parsed with
      | Choice1Of2 parsed ->
        if parsed.IsUsageRequested || List.isEmpty parsed.UnrecognizedCliParams |> not then
          ArgsUsage (parser.PrintUsage(), parsed.UnrecognizedCliParams)
        else
          let config =
            parsed.GetAllResults()
            |> Seq.fold (flip reduceKnown) baseConfig
          if parsed.Contains <@ List_Tests @> then
            ArgsList config
          elif parsed.Contains <@ Version @> then
            ArgsVersion config
          else
            ArgsRun config
      | Choice2Of2 error ->
        let upTo (s1:string) (s2:string) =
          let i = s2.IndexOf(s1)
          if i<0 then s2
          else
            s2.Substring(0,i)
        ArgsException (parser.PrintUsage(), upTo "\n" error)

  /// Prints out names of all tests for given test suite.
  let listTests test =
    test
    |> Test.toTestCodeList
    |> Seq.iter (fun t -> printfn "%s" t.name)


  /// Runs tests with the supplied config.
  /// Returns 0 if all tests passed, otherwise 1
  let runTests config (tests:Test) =
    Global.initialiseIfDefault
      { Global.defaultConfig with
          getLogger = fun name -> LiterateConsoleTarget(name, config.verbosity, consoleSemaphore = Global.semaphore()) :> Logger }
    if config.failOnFocusedTests && passesFocusTestCheck config tests |> not then
      1
    else
      let tests = config.filter tests
      match config.stress with
      | None -> runEval config tests |> Async.RunSynchronously
      | Some _ -> runStress config tests |> Async.RunSynchronously

  /// Runs all given tests with the supplied command-line options.
  /// Returns 0 if all tests passed, otherwise 1
  let runTestsWithArgs config args tests =
    match ExpectoConfig.fillFromArgs config args with
    | ArgsException (usage,message) ->
      printfn "%s\n" message
      printfn "EXPECTO version %s\n\n%s" (ExpectoConfig.expectoVersion()) usage
      1
    | ArgsUsage (usage,errors) ->
      if List.isEmpty errors |> not then
        printfn "ERROR unknown options: %s\n" (String.Join(" ",errors))

      printfn "EXPECTO version %s\n\n%s"
        (ExpectoConfig.expectoVersion()) usage

      if List.isEmpty errors then 0 else 1
    | ArgsList config ->
      let tests = config.filter tests
      listTests tests
      0
    | ArgsRun config ->
      runTests config tests
    | ArgsVersion config ->
      printfn "EXPECTO version %s\n"
        (ExpectoConfig.expectoVersion())

      runTests config tests

  /// Runs tests in this assembly with the supplied command-line options.
  /// Returns 0 if all tests passed, otherwise 1
  let runTestsInAssembly config args asm =
    let config = { config with locate = getLocation (asm) }
    testFromAssembly (asm)
    |> Option.orDefault (TestList ([], Normal))
    |> runTestsWithArgs config args

  let runTestsInThisAssembly config args =
    runTestsInAssembly config args (Assembly.GetEntryAssembly())

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