namespace Expecto

#nowarn "46"
open System
open System.Threading
open System.Threading.Tasks

[<AutoOpen>]
module Tests =
  open Expecto.CSharp
  open Impl
  open Helpers
  open Expecto.Logging

  let mutable private afterRunTestsList = []
  let private afterRunTestsListLock = obj()
  /// Add a function that will be called after all testing has finished.
  let afterRunTests f =
    lock afterRunTestsListLock (fun () ->
        afterRunTestsList <- f :: afterRunTestsList
      )
  let internal afterRunTestsInvoke() =
    lock afterRunTestsListLock (fun () ->
      let failures =
        List.rev afterRunTestsList
        |> List.choose (fun f ->
          try
            f()
            None
          with e -> Some e
        )
      match failures with
      | [] -> ()
      | l -> List.toArray l |> AggregateException |> raise
    )
  Console.CancelKeyPress |> Event.add (fun _ -> afterRunTestsInvoke())

  /// Expecto atomic printfn shadow function
  let printfn format =
    Printf.ksprintf (fun s ->
        Console.Write(s.PadRight 40 + "\n")
      ) format

  /// Expecto atomic eprintfn shadow function
  let eprintfn format =
    Printf.ksprintf (fun s ->
      Console.Error.Write(s.PadRight 40 + "\n")
    ) format

  /// The full name of the currently running test
  // The generic parameter allows it to be used like
  // a value, instead of calling testName().
  let testName<'unused> = TestNameHolder.Name

  /// Fail this test
  let inline failtest msg = raise <| AssertException msg
  /// Fail this test
  let inline failtestf fmt = Printf.ksprintf failtest fmt
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

  /// Labels the passed test with a text segment. In Expecto, tests are slash-separated (`/`), so this wraps the passed
  /// tests in such a label. Useful when you don't want lots of indentation in your tests (the code would become hard to
  /// modify and read, due to all the whitespace), and you want to do `testList "..." [ ] |> testLabel "api"`.
  let inline testLabel name test = TestLabel(name, test, Normal)

  /// Builds a test case that will be ignored by Expecto if exists focused
  /// tests and none of the parents is focused
  let inline testCase name test = TestLabel(name, TestCase (Sync test,Normal), Normal)
  /// Builds a test case with a CancellationToken that can be check for cancel
  let inline testCaseWithCancel name test = TestLabel(name, TestCase (SyncWithCancel test,Normal), Normal)
  /// Builds an async test case
  let inline testCaseAsync name test = TestLabel(name, TestCase (Async test,Normal), Normal)

  let inline private deferTaskAsAsync (taskFactory: unit -> Task<unit>) =
    // Tasks are hot, they are start right away, so we need to defer the task creation
    async {
          do! taskFactory() |> Async.AwaitTask
      }

  /// Builds an async test case from a task
  let inline testCaseTask name test = TestLabel(name, TestCase (Async (deferTaskAsAsync test),Normal), Normal)
  /// Builds a test case that will make Expecto to ignore other unfocused tests
  let inline ftestCase name test = TestLabel(name, TestCase (Sync test, Focused), Focused)
  /// Builds a test case with cancel that will make Expecto to ignore other unfocused tests
  let inline ftestCaseWithCancel name test = TestLabel(name, TestCase (SyncWithCancel test, Focused), Focused)
  /// Builds an async test case that will make Expecto to ignore other unfocused tests
  let inline ftestCaseAsync name test = TestLabel(name, TestCase (Async test, Focused), Focused)
  /// Builds an async test case from a task, that will make Expecto to ignore other unfocused tests
  let inline ftestCaseTask name test = TestLabel(name, TestCase (Async (deferTaskAsAsync test), Focused), Focused)
  /// Builds a test case that will be ignored by Expecto
  let inline ptestCase name test = TestLabel(name, TestCase (Sync test, Pending), Pending)
  /// Builds a test case with cancel that will be ignored by Expecto
  let inline ptestCaseWithCancel name test = TestLabel(name, TestCase (SyncWithCancel test, Pending), Pending)
  /// Builds an async test case that will be ignored by Expecto
  let inline ptestCaseAsync name test = TestLabel(name, TestCase (Async test, Pending), Pending)
  /// Builds an async test case from a task, that will be ignored by Expecto
  let inline ptestCaseTask name test = TestLabel(name, TestCase (Async (deferTaskAsAsync test), Pending), Pending)
  /// Test case or list needs to run sequenced. Use for any benchmark code or
  /// for tests using `Expect.isFasterThan`
  let inline testSequenced test = Sequenced (Synchronous,test)
  /// Test case or list needs to run sequenced with other tests in this group.
  let inline testSequencedGroup name test = Sequenced (SynchronousGroup name,test)

  let inline private stringify (value : 'T) =
      if typeof<'T> = typeof<string> then
        if value = Unchecked.defaultof<'T> then "null" else $"\"{value}\""
      else
        string value

  /// Applies a function to a list of values to build test cases
  let inline testFixture setup =
    Seq.map (fun (name, partialTest) ->
      testCase name (setup partialTest))
  /// Builds a theory test case
  let inline testTheory name cases test =
    let caseToTest case =
      testCase (stringify case) <| fun () ->
        test case |> ignore
    testList name (cases |> Seq.map caseToTest |> List.ofSeq)
  /// Builds a theory test case that will make Expecto to ignore other unfocused tests
  let inline ftestTheory name cases test =
    let caseToTest case =
      ftestCase (stringify case) <| fun () ->
        test case |> ignore
    ftestList name (cases |> Seq.map caseToTest |> List.ofSeq)
  /// Builds a theory test case that will be ignored by Expecto
  let inline ptestTheory name cases test =
    let caseToTest case =
      ptestCase (stringify case) <| fun () ->
        test case |> ignore
    ptestList name (cases |> Seq.map caseToTest |> List.ofSeq)

  /// Applies a value to a list of partial tests
  let inline testParam param =
    Seq.map (fun (name, partialTest) ->
      testCase name (partialTest param))

  /// Applies a value to a list of partial async tests
  let inline testParamAsync param =
    Seq.map (fun (name, partialTest) ->
      testCaseAsync name (partialTest param))

  /// Applies a value to a list of partial task tests
  let inline testParamTask param =
    Seq.map (fun (name, partialTest) ->
      testCaseTask name (partialTest param))

  /// Test case computation expression builder
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
    member __.Using(disposable: #IDisposable, f) = using disposable f
    member __.For(sequence, f) =
      for i in sequence do f i
    member __.While(gd, prog) =
      while gd() do prog()
    member __.Combine(f1, f2) = f2(); f1
    member __.Zero() = ()
    member __.Delay f = f
    member __.Run f =
      match focusState with
      | Normal -> testCase name f
      | Focused -> ftestCase name f
      | Pending -> ptestCase name f

  /// Builds a test case
  let inline test name =
    TestCaseBuilder (name, Normal)
  /// Builds a test case that will make Expecto to ignore other unfocused tests
  let inline ftest name =
    TestCaseBuilder (name, Focused)
  /// Builds a test case that will be ignored by Expecto
  let inline ptest name =
    TestCaseBuilder (name, Pending)

  /// Async test case computation expression builder
  type TestAsyncBuilder(name, focusState) =
    member __.Zero() = async.Zero()
    member __.Delay(f) = async.Delay(f)
    member __.Return(x) = async.Return(x)
    member __.ReturnFrom(x) = async.ReturnFrom(x)
    member __.Bind(p1, p2) = async.Bind(p1, p2)
    member __.Using(g, p) = async.Using(g, p)
    member __.While(gd, prog) = async.While(gd, prog)
    member __.For(e, prog) = async.For(e, prog)
    member __.Combine(p1, p2) = async.Combine(p1, p2)
    member __.TryFinally(p, cf) = async.TryFinally(p, cf)
    member __.TryWith(p, cf) = async.TryWith(p, cf)
    member __.Run f =
      match focusState with
      | Normal -> testCaseAsync name f
      | Focused -> ftestCaseAsync name f
      | Pending -> ptestCaseAsync name f

  /// Builds an async test case
  let inline testAsync name =
    TestAsyncBuilder (name, Normal)
  /// Builds an async test case that will make Expecto to ignore other unfocused tests
  let inline ftestAsync name =
    TestAsyncBuilder (name, Focused)
  /// Builds an async test case that will be ignored by Expecto
  let inline ptestAsync name =
    TestAsyncBuilder (name, Pending)
  /// Applies a function to a list of values to build async test cases
  let inline testFixtureAsync setupAsync =
    Seq.map (fun (name, partialTest) ->
      testAsync name { do! setupAsync partialTest })
  /// Builds an async theory test case
  let inline testTheoryAsync name cases test =
    let caseToTest case =
      testAsync (stringify case) { do! test case }
    testList name (cases |> Seq.map caseToTest |> List.ofSeq)
  /// Builds an async theory test case that will make Expecto to ignore other unfocused tests
  let inline ftestTheoryAsync name cases test =
    let caseToTest case =
      ftestAsync (stringify case) { do! test case }
    ftestList name (cases |> Seq.map caseToTest |> List.ofSeq)
  /// Builds an async theory test case that will be ignored by Expecto
  let inline ptestTheoryAsync name cases test =
    let caseToTest case =
      ptestAsync (stringify case) { do! test case }
    ptestList name (cases |> Seq.map caseToTest |> List.ofSeq)

  type TestTaskBuilder(name, focusState) =
    member inline __.Zero() = task.Zero()
    member inline __.Delay(f) = task.Delay(f)
    member inline __.Return(x) = task.Return(x)
    member inline __.ReturnFrom(x) = task.ReturnFrom(x)
    member inline __.Bind(p1:Task<'a>, p2:'a->_) = task.Bind(p1, p2)
    member inline __.Bind(p1:Task, p2:unit->_) = task.Bind(p1, p2)
    member inline __.Using(g, p) = task.Using(g, p)
    member inline __.While(gd, prog) = task.While(gd, prog)
    member inline __.For(e, prog) = task.For(e, prog)
    member inline __.Combine(p1, p2) = task.Combine(p1, p2)
    member inline __.TryFinally(p, cf) = task.TryFinally(p, cf)
    member inline __.TryWith(p, cf) = task.TryWith(p, cf)
    member __.Run f =
      let deferred () = task.Run f
      match focusState with
      | Normal -> testCaseTask name deferred
      | Focused -> ftestCaseTask name deferred
      | Pending -> ptestCaseTask name deferred

  [<AutoOpen>]
  module TestTaskExtensions =
    type TestTaskBuilder with
      member inline __.Bind(p1:ValueTask<'a>, p2:'a->_) = task.Bind(p1, p2)
      member inline __.Bind(p1:ValueTask, p2:unit->_) = task.Bind(p1, p2)
      member inline __.Bind(p1:Async<'a>, p2:'a->_) = task.Bind(p1, p2)

  /// Builds a task test case
  let inline testTask name =
    TestTaskBuilder (name, Normal)
  /// Builds a task test case that will make Expecto to ignore other unfocused tests
  let inline ftestTask name =
    TestTaskBuilder (name, Focused)
  /// Builds a task test case that will be ignored by Expecto
  let inline ptestTask name =
    TestTaskBuilder (name, Pending)
  /// Applies a function to a list of values to build task test cases
  let inline testFixtureTask setupTask =
    Seq.map (fun (name, partialTest) ->
      testTask name { do! setupTask partialTest })
  /// Builds a task theory test case
  let inline testTheoryTask name cases test =
    let caseToTest case =
      testTask (stringify case) { do! test case }
    testList name (cases |> Seq.map caseToTest |> List.ofSeq)
  /// Builds a task theory test case that will make Expecto to ignore other unfocused tests
  let inline ftestTheoryTask name cases test =
    let caseToTest case =
      ftestTask (stringify case) { do! test case }
    ftestList name (cases |> Seq.map caseToTest |> List.ofSeq)
  /// Builds a task theory test case that will be ignored by Expecto
  let inline ptestTheoryTask name cases test =
    let caseToTest case =
      ptestTask (stringify case) { do! test case }
    ptestList name (cases |> Seq.map caseToTest |> List.ofSeq)

  /// The default configuration for Expecto.
  let defaultConfig = ExpectoConfig.defaultConfig

  module Args =
    open FSharp.Core

    type Parser<'a> = (string[] * int * int) -> Result<'a,string> * int

    let parseOptions (options:(string * string * Parser<_>) list) (strings:string[]) =
      let rec updateUnknown unknown last length =
        if length = 0 then unknown
        else updateUnknown (strings.[last]::unknown) (last-1) (length-1)
      let rec collect isHelp unknown args paramCount i =
        if i>=0 then
          let currentArg = strings.[i]
          if currentArg = "--help" || currentArg = "-h" || currentArg = "-?" || currentArg = "/?" then
            collect true (updateUnknown unknown (i+paramCount) paramCount) args 0 (i-1)
          else
            match List.tryFind (fst3 >> (=) currentArg) options with
            | Some (option, _, parser) ->
              let arg, unknownCount = parser (strings, i+1, paramCount)
              collect isHelp
                (updateUnknown unknown (i+paramCount) unknownCount)
                (Result.mapError (fun i -> option + " " + i) arg::args) 0 (i-1)
            | None -> collect isHelp unknown args (paramCount+1) (i-1)
        else
          let unknown =
            match updateUnknown unknown (paramCount-1) paramCount with
            | [] -> None
            | l -> String.Join(" ","unknown options:" :: l) |> Some
          match isHelp, Result.sequence args, unknown with
          | false, Ok os, None -> Ok(List.rev os)
          | true, Ok _, None -> Error []
          | _, Ok _, Some u -> Error [u]
          | _, r, None -> r
          | _, Error es, Some u -> List.rev (u::es) |> Error
      collect false [] [] 0 (strings.Length-1)

    let deprecated = "Deprecated"

    let usage commandName (options: (string * string * Parser<_>) list) =
      let sb = Text.StringBuilder("Usage: ")
      let add (text:string) = sb.Append(text) |> ignore
      add commandName
      add " [options]\n\nOptions:\n"
      let maxLength =
        options |> Seq.map (fun (s,_,_) -> s.Length) |> Seq.max
      ["--help","Show this help message."]
      |> Seq.append (Seq.map (fun (s,d,_) -> s,d) options)
      |> Seq.where (snd >> (<>)deprecated)
      |> Seq.iter (fun (s,d) ->
        add "  "
        add (s.PadRight maxLength)
        add "  "
        add d
        add "\n"
      )
      sb.ToString()

    let none case : Parser<_> =
      fun (_,_,l) -> Ok case, l

    let string case : Parser<_> =
      fun (ss,i,l) ->
        if l>0 then Ok(case ss.[i]), l-1
        else Error "requires a parameter", 0

    let list (parser:_->Parser<_>) case : Parser<_> =
      fun (ss,i,l) ->
        [i..i+l-1]
        |> Result.traverse (fun j -> parser id (ss,j,1) |> fst)
        |> Result.map (fun l -> case(List.rev l))
        |> Result.mapError (fun i -> String.Join(", ", i))
        , 0

    let inline private parseWith tryParseFn case: Parser<'a> =
      fun (args, i, l) ->
        if l = 0 then Error "requires a parameter", 0
        else
          match tryParseFn args.[i] with
          | Some i -> Ok(case i), l-1
          | None -> Error("Cannot parse parameter '" + args.[i] + "'"), l-1


    let inline parse case: Parser<'a> = parseWith tryParse case
    let inline number case: Parser<'a> = parseWith tryParseNumber case
    let inline focusState case: Parser<'a> = parseWith tryParseFocusState case


  [<ReferenceEquality>]
  type SummaryHandler =
    | SummaryHandler of (TestRunSummary -> unit)

  /// The CLI arguments are the parameters that are possible to send to Expecto
  /// and change the runner's behaviour.
  type CLIArguments =
    /// Don't run the tests in parallel.
    | Sequenced
    /// Run all tests in parallel (default).
    | Parallel
    /// Set the number of parallel workers (defaults to the number of logical processors).
    | Parallel_Workers of int
    /// Set FsCheck maximum number of tests (default: 100).
    | FsCheck_Max_Tests of int
    /// Set FsCheck start size (default: 1).
    | FsCheck_Start_Size of int
    /// Set FsCheck end size (default: 100 for testing and 10,000 for stress testing).
    | FsCheck_End_Size of int
    /// Run the tests randomly for the given number of minutes.
    | Stress of float
    /// Set the time to wait in minutes after the stress test before reporting as a deadlock (default 5 mins).
    | Stress_Timeout of float
    /// Set the Stress test memory limit in MB to stop the test and report as a memory leak (default 100 MB).
    | Stress_Memory_Limit of float
    /// This will make the test runner fail if focused tests exist.
    | Fail_On_Focused_Tests
    /// Extra verbose printing. Useful to combine with --sequenced.
    | Debug
    /// Set the process name to log under (default: "Expecto").
    | Log_Name of name:string
    /// Filters the list of tests by a hierarchy that's separated by a `joinWith` operator.
    | Filter of hiera:string
    /// Filters the list of test lists by a given substring.
    | Filter_Test_List of substring:string
    /// Filters the list of test cases by a given substring.
    | Filter_Test_Case of substring:string
    /// Runs only provided list of tests.
    | Run of tests:string list
    /// Don't run tests, but prints out list of tests instead.
    | List_Tests of listStates: FocusState list
    /// Print out a summary after all tests are finished.
    | Summary
    /// Put an NUnit-like summary XML file at the given file.
    | NUnit_Summary of string
    /// Put a JUnit-like summary XML file at the given file.
    | JUnit_Summary of string
    /// Print out a summary after all tests are finished including their source code location.
    | Summary_Location
    /// Print out version information.
    | Version
    /// Allow duplicate test names.
    | Allow_Duplicate_Names
    /// Disable the spinner progress update.
    | No_Spinner
    // Set the level of colours to use. Can be 0, 8 (default) or 256.
    | Colours of int
    /// Adds a test printer.
    | Printer of TestPrinters
    /// Whether to show skipped tests in the output.
    | PrintSkippedTests of bool
    /// Sets the verbosity level.
    | Verbosity of LogLevel
    /// Append a summary handler.
    | Append_Summary_Handler of SummaryHandler
    /// Specify test names join character.
    | JoinWith of split: string

  let options = [
      "--sequenced", "Don't run the tests in parallel.", Args.none Sequenced
      "--parallel", "Run all tests in parallel (default).", Args.none Parallel
      "--parallel-workers", "Set the number of parallel workers (defaults to the number of logical processors).", Args.number Parallel_Workers
      "--stress", "Run the tests randomly for the given number of minutes.", Args.number Stress
      "--stress-timeout", "Set the time to wait in minutes after the stress test before reporting as a deadlock (default 5 mins).", Args.number Stress_Timeout
      "--stress-memory-limit", "Set the Stress test memory limit in MB to stop the test and report as a memory leak (default 100 MB).", Args.number Stress_Memory_Limit
      "--fail-on-focused-tests", "This will make the test runner fail if focused tests exist.", Args.none Fail_On_Focused_Tests
      "--debug", "Extra verbose printing. Useful to combine with --sequenced.", Args.none Debug
      "--log-name", "Set the process name to log under (default: \"Expecto\").", Args.string Log_Name
      "--filter", "Filters the list of tests by a hierarchy that's slash (/) separated.", Args.string Filter
      "--filter-test-list", "Filters the list of test lists by a given substring.", Args.string Filter_Test_List
      "--filter-test-case", "Filters the list of test cases by a given substring.", Args.string Filter_Test_Case
      "--run", "Runs only provided list of tests.", Args.list Args.string Run
      "--list-tests", "Don't run tests, but prints out list of tests instead. Lists only tests with specified state(s), or all tests if not.", Args.list (Args.focusState) List_Tests
      "--summary", "Print out a summary after all tests are finished.", Args.none Summary
      "--nunit-summary", "Put an NUnit-like summary XML file at the given file.", Args.string NUnit_Summary
      "--junit-summary", "Put a JUnit-like summary XML file at the given file.", Args.string JUnit_Summary
      "--version", "Print out version information.", Args.none Version
      "--summary-location", "Print out a summary after all tests are finished including their source code location.", Args.none Summary_Location
      "--fscheck-max-tests", "Set FsCheck maximum number of tests (default: 100).", Args.number FsCheck_Max_Tests
      "--fscheck-start-size", "Set FsCheck start size (default: 1).", Args.number FsCheck_Start_Size
      "--fscheck-end-size", "Set FsCheck end size (default: 100 for testing and 10,000 for stress testing).", Args.number FsCheck_End_Size
      "--allow-duplicate-names", "Allow duplicate test names.", Args.none Allow_Duplicate_Names
      "--colours", "Set the level of colours to use. Can be 0, 8 (default) or 256.", Args.number Colours
      "--no-spinner", "Disable the spinner progress update.", Args.none No_Spinner
      "--join-with", "Split on option. Can be \".\" (default) or \"/\".", Args.string JoinWith
  ]

  type FillFromArgsResult =
    | ArgsRun of ExpectoConfig
    | ArgsList of ExpectoConfig
    | ArgsVersion of ExpectoConfig
    | ArgsUsage of usage:string * errors:string list

  let private getTestList (s: string list) =
    match s with
    | [] | [_] -> []
    | xs -> xs.[0..s.Length-2]

  let private getTestCase (s: string list) =
    match s |> List.tryLast with
    | Some last -> last
    | None -> String.Empty

  let private foldCLIArgumentToConfig = function
    | Sequenced -> fun o -> { o with runInParallel = false }
    | Parallel -> fun o -> { o with runInParallel = true }
    | Parallel_Workers n -> fun o -> { o with parallelWorkers = n }
    | Stress n -> fun o  -> {o with
                                stress = TimeSpan.FromMinutes n |> Some
                                printer = TestPrinters.stressPrinter }
    | Stress_Timeout n -> fun o -> { o with stressTimeout = TimeSpan.FromMinutes n }
    | Stress_Memory_Limit n -> fun o -> { o with stressMemoryLimit = n }
    | Fail_On_Focused_Tests -> fun o -> { o with failOnFocusedTests = true }
    | Debug -> fun o -> { o with verbosity = LogLevel.Debug }
    | Log_Name name -> fun o -> { o with logName = Some name }
    | Filter hiera -> fun o -> {o with filter = Test.filter o.joinWith.asString (fun z -> (o.joinWith.format z).StartsWith hiera )}
    | Filter_Test_List name ->  fun o -> {o with filter = Test.filter o.joinWith.asString (fun s -> s |> getTestList |> List.exists(fun s -> s.Contains name )) }
    | Filter_Test_Case name ->  fun o -> { o with filter = Test.filter o.joinWith.asString (fun s -> s |> getTestCase |> fun s -> s.Contains name )}
    | Run tests -> fun o -> {o with filter = Test.filter o.joinWith.asString (fun s -> tests |> List.exists ((=) (o.joinWith.format s)) )}
    | List_Tests states -> fun o -> { o with listStates = states }
    | Summary -> fun o -> {o with printer = TestPrinters.summaryPrinter o.printer}
    | NUnit_Summary path -> fun o -> o.AddNUnitSummary(path)
    | JUnit_Summary path -> fun o -> o.AddJUnitSummary(path)
    | Version -> id
    | Summary_Location -> fun o -> {o with printer = TestPrinters.summaryWithLocationPrinter o.printer}
    | FsCheck_Max_Tests n -> fun o -> {o with fsCheckMaxTests = n }
    | FsCheck_Start_Size n -> fun o -> {o with fsCheckStartSize = n }
    | FsCheck_End_Size n -> fun o -> {o with fsCheckEndSize = Some n }
    | Allow_Duplicate_Names -> fun o -> { o with allowDuplicateNames = true }
    | No_Spinner -> fun o -> { o with noSpinner = true }
    | Colours i -> fun o -> { o with colour =
                                      if i >= 256 then Colour256
                                      elif i >= 8 then Colour8
                                      else Colour0
                  }
    | JoinWith s -> fun o -> { o with joinWith =
                                      match s with
                                      | "." -> JoinWith.Dot
                                      | "/" -> JoinWith.Slash
                                      | _ -> JoinWith.Dot
                  }
    | Printer p -> fun o -> { o with printer = p }
    | PrintSkippedTests b -> fun o -> { o with printSkippedTests = b }
    | Verbosity l -> fun o -> { o with verbosity = l }
    | Append_Summary_Handler (SummaryHandler h) -> fun o -> o.appendSummaryHandler h

  [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
  module ExpectoConfig =

    /// Parses command-line arguments into a config. This allows you to
    /// override the config from the command line, rather than having
    /// to go into the compiled code to change how they are being run.
    /// Also checks if tests should be run or only listed
    let fillFromArgs baseConfig args =
      match Args.parseOptions options args with
      | Ok cliArguments ->
          let config =
            List.fold (fun s a -> foldCLIArgumentToConfig a s) baseConfig cliArguments
          if List.exists(function List_Tests _ -> true | _ -> false) cliArguments then
            ArgsList config
          elif List.contains Version cliArguments then
            ArgsVersion config
          else
            ArgsRun config
      | Result.Error errors ->
        let commandName =
          Environment.GetCommandLineArgs().[0]
          |> IO.Path.GetFileName
          |> fun f -> if f.EndsWith(".dll") then "dotnet " + f else f
        ArgsUsage (Args.usage commandName options, errors)

  /// Prints out names of all tests for given test suite.
  let listTests config test =
    let toStateChar state =
      match state with
      | Normal  -> "N"
      | Pending -> "P"
      | Focused -> "F"

    let tests =
      Test.toTestCodeList test
      |> Seq.filter(fun t -> List.isEmpty config.listStates ||
                             List.contains t.state config.listStates)
      |> Seq.map(fun t -> toStateChar t.state, config.joinWith.format t.name)
      |> Seq.toList

    let hideState = tests |> List.exists(fun (stateChar,_) -> stateChar <> "N") |> not

    let result = System.Text.StringBuilder()
    tests
    |> Seq.iter (fun (stateChar, name) ->
      if hideState then
        Printf.bprintf result "\n%s" name
      else
        Printf.bprintf result "\n%s %s" stateChar name
    )
    Printf.bprintf result "\n"

    logger.logWithAck Info (
      Message.eventX "{result}"
      >> Message.setField "result" (string result)
    )
    |> Async.RunSynchronously

  /// Prints out names of all tests for given test suite.
  let duplicatedNames (join: JoinWith) test =
    Test.toTestCodeList test
    |> Seq.toList
    |> List.groupBy (fun t -> (join.format t.name))
    |> List.choose (function
        | _, x :: _ :: _ -> Some x.name
        | _ -> None
    )

  /// Runs all given tests with the supplied typed command-line options.
  /// Returns 0 if all tests passed, otherwise 1
  let runTestsWithCLIArgsAndCancel (ct:CancellationToken) cliArgs args tests =
    let runTestsWithCancel (ct:CancellationToken) config (tests:Test) =
      ANSIOutputWriter.setColourLevel config.colour
      Global.initialiseIfDefault
        { Global.defaultConfig with
            getLogger = fun name ->
              LiterateConsoleTarget(
                name, config.verbosity,
                consoleSemaphore = Global.semaphore()) :> Logger
        }
      config.logName |> Option.iter setLogName
      if config.failOnFocusedTests && passesFocusTestCheck config tests |> not then
        1
      else
        let fTests = config.filter tests
        let duplicates = lazy duplicatedNames config.joinWith fTests
        if config.allowDuplicateNames || List.isEmpty duplicates.Value then
          let retCode =
            match config.stress with
            | None -> runEvalWithCancel ct config fTests |> Async.RunSynchronously
            | Some _ -> runStressWithCancel ct config fTests |> Async.RunSynchronously
          afterRunTestsInvoke()
          retCode
        else
          sprintf "Found duplicated test names, these names are: %A" duplicates.Value
          |> config.printer.info
          |> Async.RunSynchronously
          ANSIOutputWriter.close()
          1

    let config =
      Seq.fold (fun s a -> foldCLIArgumentToConfig a s)
        ExpectoConfig.defaultConfig cliArgs

    match ExpectoConfig.fillFromArgs config args with
    | ArgsUsage (usage, errors) ->
      if not (List.isEmpty errors) then
        printfn "ERROR: %s\n" (String.Join(" ",errors))
      printfn "EXPECTO! v%s\n\n%s" expectoVersion usage
      if List.isEmpty errors then 0 else 1
    | ArgsList config ->
      listTests config tests
      0
    | ArgsRun config ->
      runTestsWithCancel ct config tests
    | ArgsVersion config ->
      printfn "EXPECTO! v%s\n" expectoVersion
      runTestsWithCancel ct config tests

  /// Runs all given tests with the supplied typed command-line options.
  /// Returns 0 if all tests passed, otherwise 1
  let runTestsWithCLIArgs cliArgs args tests =
    runTestsWithCLIArgsAndCancel CancellationToken.None cliArgs args tests

  /// Runs tests in this assembly with the supplied command-line options.
  /// Returns 0 if all tests passed, otherwise 1
  let runTestsInAssemblyWithCLIArgsAndCancel (ct:CancellationToken) cliArgs args =
    let tests = testFromThisAssembly() |> Option.orDefault (TestList ([], Normal))
    runTestsWithCLIArgsAndCancel ct cliArgs args tests

  /// Runs tests in this assembly with the supplied command-line options.
  /// Returns 0 if all tests passed, otherwise 1
  let runTestsInAssemblyWithCLIArgs cliArgs args =
    runTestsInAssemblyWithCLIArgsAndCancel CancellationToken.None cliArgs args
