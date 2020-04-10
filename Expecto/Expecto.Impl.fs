namespace Expecto

open System
open System.Diagnostics
open System.Reflection
open System.Threading
open Expecto.Logging
open Expecto.Logging.Message
open Helpers
open Mono.Cecil

// TODO: make internal?
module Impl =

  let mutable logger = Log.create "Expecto"
  let setLogName name = logger <- Log.create name

  let rec private exnWithInnerMsg (ex: exn) msg =
    let currentMsg =
      msg + (sprintf "%s%s" Environment.NewLine (ex.ToString()))
    if isNull ex.InnerException then
      currentMsg
    else
      exnWithInnerMsg ex.InnerException currentMsg

  type JoinWith =
    | Dot
    | Slash
    member x.asString =
      match x with
      | Dot -> "."
      | _ -> "/"

    member x.format (parts: string list) =
      let by = x.asString
      String.concat by parts

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
      | Error e -> "Exception: " + exnWithInnerMsg e ""
    member x.tag =
      match x with
      | Passed -> 0
      | Ignored _ -> 1
      | Failed _ -> 2
      | Error _ -> 3

    member x.order =
      match x with
      | Ignored _ -> 0
      | Passed -> 1
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
      maxDuration   : float }

    member x.duration = TimeSpan.FromMilliseconds x.meanDuration
    static member single result duration =
      { result        = result
        count         = 1
        meanDuration  = duration
        maxDuration   = duration }
    static member (+) (s:TestSummary, (r,x): TestResult*float) =
      { result        = TestResult.max s.result r
        count         = s.count + 1
        meanDuration  =
          s.meanDuration + (x-s.meanDuration)/float(s.count + 1)
        maxDuration   = max s.maxDuration x }

  type TestRunSummary =
    { results     : (FlatTest * TestSummary) list
      duration    : TimeSpan
      maxMemory   : int64
      memoryLimit : int64
      timedOut    : FlatTest list
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

  let createSummaryMessage joinWith (summary: TestRunSummary) =
    let handleLineBreaks (elements:(FlatTest*TestSummary) seq) =
        elements
        |> Seq.map (fun (n,_) -> "\n\t" + n.fullName joinWith)
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
      [ passedCount; ignoredCount; failedCount; erroredCount ]
      |> List.map (fun x -> x.ToString().Length)
      |> List.max

    let align (s: string) offset = s.PadLeft(offset + digits)

    eventX "EXPECTO?! Summary...\nPassed: {passedCount}{passed}\nIgnored: {ignoredCount}{ignored}\nFailed: {failedCount}{failed}\nErrored: {erroredCount}{errored}"
    >> setField "passed" passed
    >> setField "passedCount" (align passedCount 1)
    >> setField "ignored" ignored
    >> setField "ignoredCount" (align ignoredCount 0)
    >> setField "failed" failed
    >> setField "failedCount" (align failedCount 1)
    >> setField "errored" errored
    >> setField "erroredCount" (align erroredCount 0)

  let createSummaryText joinWith (summary: TestRunSummary) =
    createSummaryMessage joinWith summary Info
    |> Formatting.defaultFormatter

  let logSummary (joinWith: JoinWith) (summary: TestRunSummary) =
    let split = joinWith.asString
    createSummaryMessage split summary
    |> logger.logWithAck Info

  let logSummaryWithLocation (join: JoinWith) locate (summary: TestRunSummary) =
    let handleLineBreaks (elements:(FlatTest*TestSummary) seq) =
      let format (n:FlatTest,_) =
        let location = locate n.test
        let name = join.format n.name
        sprintf "%s [%s:%d]" name location.sourcePath location.lineNumber

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
  [<ReferenceEquality>]
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
      let name = config.joinWith.format test.name
      match result.result with
      | Passed -> config.printer.passed name result.duration
      | Failed message -> config.printer.failed name message result.duration
      | Ignored message -> config.printer.ignored name message
      | Error e -> config.printer.exn name e result.duration

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
          async {
            do! logger.logWithAck LogLevel.Error (
                  eventX "{testName} failed in {duration}. {message}"
                  >> setField "testName" n
                  >> setField "duration" d
                  >> setField "message" m)
            ANSIOutputWriter.flush ()
          }

        exn = fun n e d ->
          async {
            do! logger.logWithAck LogLevel.Error (
                  eventX "{testName} errored in {duration}"
                  >> setField "testName" n
                  >> setField "duration" d
                  >> addExn e)
            ANSIOutputWriter.flush ()
          }

        summary = fun _config summary ->
          let splitSign = _config.joinWith.asString
          let spirit =
            if summary.successful then "Success!" else String.Empty
          let commonAncestor =
            let rec loop ancestor (descendants : string list) =
              match descendants with
              | [] -> ancestor
              | hd::tl when hd.StartsWith(ancestor)->
                loop ancestor tl
              | _ ->
                if ancestor.Contains(splitSign) then
                  loop (ancestor.Substring(0, ancestor.LastIndexOf splitSign)) descendants
                else
                  "miscellaneous"

            let parentNames =
              summary.results
              |> List.map (fun (flatTest, _)  ->
                if flatTest.name.Length > 1 then
                  let size = flatTest.name.Length - 1
                  _config.joinWith.format flatTest.name.[0..size]
                else
                  _config.joinWith.format flatTest.name )

            match parentNames with
            | [x] -> x
            | hd::tl ->
              loop hd tl
            | _ -> "miscellaneous" //we can't get here

          logger.logWithAck Info (
            eventX "EXPECTO! {total} tests run in {duration} for {name} – {passes} passed, {ignores} ignored, {failures} failed, {errors} errored. {spirit}"
            >> setField "total" (List.sumBy (fun (_,r) -> if r.result.isIgnored then 0 else r.count) summary.results |> commaString)
            >> setField "name" commonAncestor
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
            let getName (name: string list) =
              config.joinWith.format name
            let printResults =
              List.map (fun (t,r) -> TestPrinters.printResult config t r) summary.results
              |> Async.foldSequentially (fun _ _ -> ()) ()
            let result =
              if summary.maxMemory > summary.memoryLimit then
                logger.logWithAck LogLevel.Error (
                  eventX "Maximum memory usage was {memory} KB and exceeded the limit set at {limit} KB.\nRunning tests:\n\t{timeout}"
                  >> setField "memory" (summary.maxMemory / 1024L |> int |> commaString)
                  >> setField "limit" (summary.memoryLimit / 1024L |> int |> commaString)
                  >> setField "timeout" (summary.timedOut |> Seq.map (fun t -> getName t.name) |> String.concat "\n\t"))
              elif List.isEmpty summary.timedOut then
                logger.logWithAck Info (
                  eventX "Maximum memory usage was {memory} KB (limit set at {limit} KB)."
                  >> setField "memory" (summary.maxMemory / 1024L |> int |> commaString)
                  >> setField "limit" (summary.memoryLimit / 1024L |> int |> commaString))
              else
                logger.logWithAck LogLevel.Error (
                  eventX "Deadlock timeout running tests:\n\t{timeout}"
                  >> setField "timeout" (summary.timedOut |> Seq.map (fun t -> getName t.name) |> String.concat "\n\t"))
            async {
              do! printResults
              do! result
              do! TestPrinters.defaultPrinter.summary config summary
            }
          }

    static member summaryPrinter innerPrinter =
      { innerPrinter with
          summary = fun config summary ->
            innerPrinter.summary config summary
            |> Async.bind (fun () -> logSummary config.joinWith summary) }

    static member summaryWithLocationPrinter innerPrinter =
      { innerPrinter with
          summary = fun config summary ->
            innerPrinter.summary config summary
            |> Async.bind (fun () -> logSummaryWithLocation config.joinWith config.locate summary) }

    static member teamCityPrinter innerPrinter =
      let formatName (n:string) =
        n.Replace( " ", "_" )

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
            "flowId", formatName n
            "name", formatName n ] }

        passed = fun n d -> async {
          do! innerPrinter.passed n d
          tcLog "testFinished" [
            "flowId", formatName n
            "name", formatName n
            "duration", d.TotalMilliseconds |> int |> string ] }

        info = fun s ->
          innerPrinter.info s

        ignored = fun n m -> async {
          do! innerPrinter.ignored n m
          tcLog "testIgnored" [
            "flowId", formatName n
            "name", formatName n
            "message", m ] }

        failed = fun n m d -> async {
          do! innerPrinter.failed n m d
          tcLog "testFailed" [
            "flowId", formatName n
            "name", formatName n
            "message", m ]
          tcLog "testFinished" [
            "flowId", formatName n
            "name", formatName n
            "duration", d.TotalMilliseconds |> int |> string ] }

        exn = fun n e d -> async {
          do! innerPrinter.beforeEach n
          tcLog "testFailed" [
            "flowId", formatName n
            "name", formatName n
            "message", e.Message
            "details", e.StackTrace ]
          tcLog "testFinished" [
            "flowId", formatName n
            "name", formatName n
            "duration", d.TotalMilliseconds |> int |> string ] }

        summary = fun c s -> async {
          do! innerPrinter.summary c s
          tcLog "testSuiteFinished" [
            "name", "ExpectoTestSuite" ] } }
    static member internal mergePrinters (first:TestPrinters, second:TestPrinters) =
      let runTwoAsyncs a b = async {
        do! a
        do! b
      }
      { beforeRun   = fun _tests -> runTwoAsyncs (first.beforeRun _tests) (second.beforeRun _tests)
        beforeEach  = fun n -> runTwoAsyncs (first.beforeEach n) (second.beforeEach n)
        info        = fun s -> runTwoAsyncs (first.info s) (second.info s)
        passed      = fun n d -> runTwoAsyncs (first.passed n d) (second.passed n d)
        ignored     = fun n m -> runTwoAsyncs (first.ignored n m) (second.ignored n m)
        failed      = fun n m d -> runTwoAsyncs (first.failed n m d) (second.failed n m d)
        exn         = fun n e d -> runTwoAsyncs (first.exn n e d) (second.exn n e d)
        summary     = fun config summary -> runTwoAsyncs (first.summary config summary) (second.summary config summary)
      }

  // Runner options
  and ExpectoConfig =
    { /// Whether to run the tests in parallel. Defaults to
      /// true, because your code should not mutate global
      /// state by default.
      runInParallel : bool
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
      /// a memory leak (default 100 MB).
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
      /// Process name to log under (default: "Expecto")
      logName : string option
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
      /// Depricated. Will be removed on next major release.
      mySpiritIsWeak: bool
      /// Allows duplicate test names.
      allowDuplicateNames: bool
      /// Disable spinner progress update.
      noSpinner: bool
      /// Set the level of colours to use.
      colour: ColourLevel
      /// Split test names by `.` or `/`
      joinWith: JoinWith
    }
    static member defaultConfig =
      { runInParallel = true
        parallelWorkers = Environment.ProcessorCount
        stress = None
        stressTimeout = TimeSpan.FromMinutes 5.0
        stressMemoryLimit = 100.0
        filter = id
        failOnFocusedTests = false
        printer =
          let tc = Environment.GetEnvironmentVariable "TEAMCITY_PROJECT_NAME"
          if isNull tc then
            TestPrinters.defaultPrinter
          else
            TestPrinters.teamCityPrinter TestPrinters.defaultPrinter
        verbosity = Info
        logName = None
        locate = fun _ -> SourceLocation.empty
        fsCheckMaxTests = 100
        fsCheckStartSize = 1
        fsCheckEndSize = None
        mySpiritIsWeak = false
        allowDuplicateNames = false
        noSpinner = false
        colour = Colour8
        joinWith = JoinWith.Dot
      }

    member x.appendSummaryHandler handleSummary =
      { x with
          printer =
            { x.printer with
                summary = fun config summary -> async {
                  do! x.printer.summary config summary
                  handleSummary summary
                }
              }
      }

  let execTestAsync (ct:CancellationToken) config (test:FlatTest) : Async<TestSummary> =
    async {
      let w = Stopwatch.StartNew()
      try
        match test.shouldSkipEvaluation with
        | Some ignoredMessage ->
          return TestSummary.single (Ignored ignoredMessage) 0.0
        | None ->
          TestNameHolder.Name <- config.joinWith.format test.name
          match test.test with
          | Sync test ->
            test()
          | SyncWithCancel test ->
            test ct
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
            "\n" + e.Message + "\n" +
            (e.StackTrace.Split('\n')
             |> Seq.skipWhile (fun l -> l.StartsWith("   at Expecto.Expect."))
             |> Seq.truncate 5
             |> String.concat "\n")
          return TestSummary.single (Failed msg) (float w.ElapsedMilliseconds)
        | :? FailedException as e ->
          w.Stop()
          return TestSummary.single (Failed ("\n"+e.Message)) (float w.ElapsedMilliseconds)
        | :? IgnoreException as e ->
          w.Stop()
          return TestSummary.single (Ignored e.Message) (float w.ElapsedMilliseconds)
        | :? AggregateException as e when e.InnerExceptions.Count = 1 ->
          w.Stop()
          if e.InnerException :? IgnoreException then
            return TestSummary.single (Ignored e.InnerException.Message) (float w.ElapsedMilliseconds)
          else
            return TestSummary.single (Error e.InnerException) (float w.ElapsedMilliseconds)
        | e ->
          w.Stop()
          return TestSummary.single (Error e) (float w.ElapsedMilliseconds)
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
  let evalTestsWithCancel (ct:CancellationToken) config test progressStarted =
    async {

      let tests = Test.toTestCodeList test
      let testLength =
        tests
        |> Seq.where (fun t -> Option.isNone t.shouldSkipEvaluation)
        |> Seq.length

      let testsCompleted = ref 0

      let evalTestAsync (test:FlatTest) =

        let beforeEach (test:FlatTest) =
          let name = config.joinWith.format test.name
          config.printer.beforeEach name

        async {
          let! beforeAsync = beforeEach test |> Async.StartChild
          let! result = execTestAsync ct config test
          do! beforeAsync
          do! TestPrinters.printResult config test result

          if progressStarted && Option.isNone test.shouldSkipEvaluation then
            Fraction (Interlocked.Increment testsCompleted, testLength)
            |> ProgressIndicator.update

          return test,result
        }

      let inline cons xs x = x::xs

      if not config.runInParallel ||
         config.parallelWorkers = 1 ||
         List.forall (fun t -> t.sequenced=Synchronous) tests then
        return!
          List.map evalTestAsync tests
          |> Async.foldSequentiallyWithCancel ct cons []
      else
        let sequenced =
          List.filter (fun t -> t.sequenced=Synchronous) tests
          |> List.map evalTestAsync

        let runInParallel =
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
              | [test] -> Async.map List.singleton test
              | l -> Async.foldSequentiallyWithCancel ct cons [] l
            )

        let! parallelResults =
          let noWorkers = numberOfWorkers false config
          Async.foldParallelWithCancel noWorkers ct (@) [] runInParallel

        if List.isEmpty sequenced |> not && List.isEmpty runInParallel |> not then
          do! config.printer.info "Starting sequenced tests..."

        let! results = Async.foldSequentiallyWithCancel ct cons parallelResults sequenced
        return List.sortBy (fun (t,_) ->
                  List.tryFindIndex (LanguagePrimitives.PhysicalEquality t) tests
               ) results
      }

  /// Evaluates tests.
  let evalTests config test =
    evalTestsWithCancel CancellationToken.None config test false

  let evalTestsSilent test =
    let config =
      { ExpectoConfig.defaultConfig with
          runInParallel = false
          verbosity = LogLevel.Fatal
          printer = TestPrinters.silent
      }
    evalTests config test

  /// Runs tests, returns error code
  let runEvalWithCancel (ct:CancellationToken) config test =
    async {
      do! config.printer.beforeRun test

      let progressStarted =
        if config.noSpinner then false
        else
          ProgressIndicator.text "Expecto Running... "
          ProgressIndicator.start()


      let w = Stopwatch.StartNew()
      let! results = evalTestsWithCancel ct config test progressStarted
      w.Stop()
      let testSummary = {
        results = results
        duration = w.Elapsed
        maxMemory = 0L
        memoryLimit = 0L
        timedOut = []
      }
      do! config.printer.summary config testSummary

      if progressStarted then
        ProgressIndicator.stop ()

      ANSIOutputWriter.close ()

      return testSummary.errorCode
    }

  /// Runs tests, returns error code
  let runEval config test =
    runEvalWithCancel CancellationToken.None config test

  let runStressWithCancel (ct: CancellationToken) config test =
    async {
      do! config.printer.beforeRun test

      let progressStarted =
        if config.noSpinner then false
        else
          ProgressIndicator.text "Expecto Running... "
          ProgressIndicator.start()

      let tests =
        Test.toTestCodeList test
        |> List.filter (fun t -> Option.isNone t.shouldSkipEvaluation)

      let memoryLimit =
        config.stressMemoryLimit * 1024.0 * 1024.0 |> int64

      let evalTestAsync test =
        execTestAsync ct config test |> Async.map (addFst test)

      let rand = Random()

      let randNext tests =
        let next = List.length tests |> rand.Next
        List.item next tests

      let totalTicks =
        config.stress.Value.TotalSeconds * float Stopwatch.Frequency
        |> int64

      let finishTime =
        lazy
        totalTicks |> (+) (Stopwatch.GetTimestamp())

      let asyncRun foldRunner (runningTests: ResizeArray<_>,
                               results,
                               maxMemory) =
        let cancel = new CancellationTokenSource()

        let folder (runningTests: ResizeArray<_>, results: ResizeMap<_,_>, maxMemory)
                   (test, result) =

          runningTests.Remove test |> ignore

          results.[test] <-
            match results.TryGetValue test with
            | true, existing ->
              existing + (result.result, result.meanDuration)
            | false, _ ->
              result

          let maxMemory = GC.GetTotalMemory false |> max maxMemory

          if maxMemory > memoryLimit then
            cancel.Cancel()

          runningTests, results, maxMemory

        Async.Start(async {
          let finishMilliseconds =
            max (finishTime.Value - Stopwatch.GetTimestamp()) 0L
            * 1000L / Stopwatch.Frequency
          let timeout =
            int finishMilliseconds + int config.stressTimeout.TotalMilliseconds
          do! Async.Sleep timeout
          cancel.Cancel()
        }, cancel.Token)

        Seq.takeWhile (fun test ->
          let now = Stopwatch.GetTimestamp()

          if progressStarted then
            100 - int((finishTime.Value - now) * 100L / totalTicks)
            |> Percent
            |> ProgressIndicator.update

          if now < finishTime.Value
              && not ct.IsCancellationRequested then
            runningTests.Add test
            true
          else
            false )
        >> Seq.map evalTestAsync
        >> foldRunner cancel.Token folder (runningTests,results,maxMemory)

      let initial = ResizeArray(), ResizeMap(), GC.GetTotalMemory false

      let w = Stopwatch.StartNew()

      let! runningTests,results,maxMemory =
        if not config.runInParallel ||
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
                  Stopwatch.GetTimestamp() > finishTime.Value then
                 async.Return (runningTests,results,maxMemory)
               else
                 let runInParallel =
                   List.filter (fun t -> t.sequenced<>Synchronous) tests
                 Seq.initInfinite (fun _ -> randNext runInParallel)
                 |> Seq.append runInParallel
                 |> Seq.filter (fun test ->
                      let s = test.sequenced
                      s=InParallel ||
                      not(Seq.exists (fun t -> t.sequenced=s) runningTests)
                    )
                 |> asyncRun
                      (Async.foldParallelWithCancel (numberOfWorkers true config))
                      (runningTests,results,maxMemory)
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

      if progressStarted then
        ProgressIndicator.stop()

      ANSIOutputWriter.close()

      return testSummary.errorCode
    }

  let runStress config test =
    runStressWithCancel CancellationToken.None config test

  let testFromMember (mi: MemberInfo) : Test option =
    let inline unboxTest v =
      if isNull v then
        "Test is null. Assembly may not be initialized. Consider adding an [<EntryPoint>] or making it a library/classlib."
        |> NullTestDiscoveryException |> raise
      else unbox v
    let getTestFromMemberInfo focusedState =
      match box mi with
      | :? FieldInfo as m ->
        if m.FieldType = typeof<Test> then Some(focusedState, m.GetValue(null) |> unboxTest)
        else None
      | :? MethodInfo as m ->
        if m.ReturnType = typeof<Test> then Some(focusedState, m.Invoke(null, null) |> unboxTest)
        else None
      | :? PropertyInfo as m ->
        if m.PropertyType = typeof<Test> then Some(focusedState, m.GetValue(null, null) |> unboxTest)
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
      [ t.GetTypeInfo().GetMethods bindingFlags |> asMembers
        t.GetTypeInfo().GetProperties bindingFlags |> asMembers
        t.GetTypeInfo().GetFields bindingFlags |> asMembers ]
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
        if t.GetTypeInfo().BaseType |> isNull || t.GetTypeInfo().BaseType = typeof<obj> then
          t
        else
          findBase (t.GetTypeInfo().BaseType)
      findBase t
    baseType.GetTypeInfo().IsGenericType && baseType.GetTypeInfo().GetGenericTypeDefinition() = typedefof<FSharpFunc<unit, unit>>

  let getFuncTypeToUse (testFunc:unit->unit) (asm:Assembly) =
    let t = testFunc.GetType()
    if t.GetTypeInfo().Assembly.FullName = asm.FullName then
      t
    else
      let nestedFunc =
        t.GetTypeInfo().GetFields()
        |> Seq.tryFind (fun f -> isFsharpFuncType f.FieldType)
      match nestedFunc with
      | Some f -> f.GetValue(testFunc).GetType()
      | None -> t

  let getMethodName asm testCode =
    match testCode with
    | Sync test ->
      let t = getFuncTypeToUse test asm
      let m = t.GetTypeInfo().GetMethods () |> Seq.find (fun m -> (m.Name = "Invoke") && (m.DeclaringType = t))
      (t.FullName, m.Name)
    | SyncWithCancel _ ->
      ("Unknown SyncWithCancel", "Unknown SyncWithCancel")
    | Async _ | AsyncFsCheck _ ->
      ("Unknown Async", "Unknown Async")

  // Ported from
  // https://github.com/adamchester/expecto-adapter/blob/885fc9fff0/src/Expecto.VisualStudio.TestAdapter/SourceLocation.fs
  let getSourceLocation (asm: Assembly) className methodName =
    let lineNumberIndicatingHiddenLine = 0xfeefee
    let getEcma335TypeName (clrTypeName:string) = clrTypeName.Replace("+", "/")

    let types =
      let readerParams = new ReaderParameters( ReadSymbols = true )
      let moduleDefinition = ModuleDefinition.ReadModule(asm.Location, readerParams)

      seq { for t in moduleDefinition.GetTypes() -> (t.FullName, t) }
      |> Map.ofSeq

    let getMethods typeName =
      match types.TryFind (getEcma335TypeName typeName) with
      | Some t -> Some (t.Methods)
      | _ -> None

    let getFirstOrDefaultSequencePoint (m:MethodDefinition) =
      m.Body.Instructions
      |> Seq.tryPick (fun i ->
        let sp = m.DebugInformation.GetSequencePoint i
        if isNull sp |> not && sp.StartLine <> lineNumberIndicatingHiddenLine then
          Some sp else None)

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
    with :? IO.FileNotFoundException ->
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
        logger.logWithAck LogLevel.Error (
          eventX "It was requested that no focused tests exist, but yet there are {count} focused tests found."
          >> setField "count" focused.Length)
        |> Async.StartImmediate
        ANSIOutputWriter.flush ()
      false
