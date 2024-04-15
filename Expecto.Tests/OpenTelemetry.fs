namespace Expecto

module OpenTelemetry =
  open System
  open System.Diagnostics
  open System.Collections.Generic
  open System.Threading
  open Impl

  module internal Activity =
    let inline setStatus (status : ActivityStatusCode) (span : Activity) =
      if isNull span |> not then
        span.SetStatus(status) |> ignore

    let inline setExn (e : exn) (span : Activity) =
      if isNull span |> not then
        let tags =
            ActivityTagsCollection(
                seq {
                    KeyValuePair("exception.type", box (e.GetType().Name))
                    KeyValuePair("exception.stacktrace", box (e.ToString()))
                    if not <| String.IsNullOrEmpty(e.Message) then
                        KeyValuePair("exception.message", box e.Message)
                }
            )

        ActivityEvent("exception", tags = tags)
        |> span.AddEvent
        |> ignore

    let inline setExnMarkFailed (e : exn) (span : Activity) =
      if isNull span |> not then
        setExn e span
        span  |> setStatus ActivityStatusCode.Error

    let setSourceLocation (sourceLoc : SourceLocation) (span : Activity) =
      if isNull span |> not && sourceLoc <> SourceLocation.empty then
        span.SetTag("code.lineno", sourceLoc.lineNumber) |> ignore
        span.SetTag("code.filepath", sourceLoc.sourcePath) |> ignore

    let inline addOutcome (result : TestResult) (span : Activity) =
      if isNull span |> not then
        span.SetTag("test.result.status", result.tag) |> ignore
        span.SetTag("test.result.message", result) |> ignore

    let inline start (span : Activity) =
      if isNull span |> not then
        span.Start() |> ignore
      span

    let inline stop (span : Activity) =
      if isNull span |> not then
        span.Stop() |> ignore

    let inline createActivity (name : string) (source : ActivitySource option) =
      match source with
      | Some source when not(isNull source) -> source.CreateActivity(name, ActivityKind.Internal)
      | _ -> null

  open Activity

  open System.Runtime.ExceptionServices

  let inline internal reraiseAnywhere<'a> (e: exn) : 'a =
      ExceptionDispatchInfo.Capture(e).Throw()
      Unchecked.defaultof<'a>

  module TestResult =
    let ofException  (e:Exception) : TestResult =
      match e with
      | :? AssertException as e ->
        let msg =
          "\n" + e.Message + "\n" +
          (e.StackTrace.Split('\n')
          |> Seq.skipWhile (fun l -> l.StartsWith("   at Expecto.Expect."))
          |> Seq.truncate 5
          |> String.concat "\n")
        Failed msg

      | :? FailedException as e ->
        Failed ("\n"+e.Message)
      | :? IgnoreException as e ->
        Ignored e.Message
      | :? AggregateException as e when e.InnerExceptions.Count = 1 ->
        if e.InnerException :? IgnoreException then
          Ignored e.InnerException.Message
        else
          Error e.InnerException
      | e ->
        Error e


  let addExceptionOutcomeToSpan (span: Activity) (e: Exception) =
    let testResult = TestResult.ofException e

    addOutcome testResult span
    match testResult with
    | Ignored _ ->
      setExn e span
    | _ ->
      setExnMarkFailed e span
  let wrapCodeWithSpan (span: Activity) (test: TestCode) =
    match test with
    | Sync test ->
      TestCode.Sync (fun () ->
        try
          start span
          test ()
          stop span
          addOutcome Passed span
          setStatus ActivityStatusCode.Ok span
        with
        | e ->
          addExceptionOutcomeToSpan span e
          reraiseAnywhere e
      )

    | Async test ->
      TestCode.Async (async {
        try
          start span
          do! test
          stop span
          addOutcome Passed span
          setStatus ActivityStatusCode.Ok span
        with
        | e ->
          addExceptionOutcomeToSpan span e
          reraiseAnywhere e
      })
    | AsyncFsCheck (testConfig, stressConfig, test) ->
      TestCode.AsyncFsCheck (testConfig, stressConfig, fun fsCheckConfig -> async {
        try
          start span
          do! test fsCheckConfig
          stop span
          addOutcome Passed span
          setStatus ActivityStatusCode.Ok span
        with
        | e ->
          addExceptionOutcomeToSpan span e
          reraiseAnywhere e
      })
    | SyncWithCancel test->
      TestCode.SyncWithCancel (fun ct ->
        try
          start span
          test ct
          stop span
          addOutcome Passed span
          setStatus ActivityStatusCode.Ok span
        with
        | e ->
          addExceptionOutcomeToSpan span e
          reraiseAnywhere e
      )


  let addOpenTelemetry_SpanPerTest (config: ExpectoConfig) (activitySource: ActivitySource) (rootTest: Test) : Test =

    rootTest
    |> Test.toTestCodeList
    |> List.map (fun test ->
      let span = createActivity (config.joinWith.format test.name) (Some activitySource)
      span |> setSourceLocation (config.locate test.test)
      {test with test = wrapCodeWithSpan span test.test}
    )
    |> Test.fromFlatTests config.joinWith.asString
