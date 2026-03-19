namespace Expecto

module OpenTelemetry =
  open System
  open System.Diagnostics
  open System.Collections.Generic
  open Impl
  open System.Runtime.CompilerServices
  type Activity with
    /// <summary>Sets code semantic conventions for <c>code.function.name</c>, <c>code.filepath</c>, and <c>code.lineno</c> </summary>
    /// <param name="name_space">Optional: The current namespace. Will default to using <c>Reflection.MethodBase.GetCurrentMethod().DeclaringType</c></param>
    /// <param name="memberName">Optional: The current function  Don't set this. This uses <c>CallerMemberName.</c></param>
    /// <param name="path">Optional: The current filepath. Don't set this. This uses <c>CallerFilePath.</c></param>
    /// <param name="line">Optional: The current line number. Don't set this. This uses <c>CallerLineNumber.</c></param>
    member inline x.SetSource(
      ?nameSpace : string,
      [<CallerMemberName>] ?memberName: string,
      [<CallerFilePath>] ?path: string,
      [<CallerLineNumber>] ?line: int) =
        if not (isNull x) then
          if x.GetTagItem "code.function.name" = null then
            let nameSpace =
              nameSpace
              |> Option.defaultWith (fun () ->
                Reflection.MethodBase.GetCurrentMethod().DeclaringType.FullName.Split("+") // F# has + in type names that refer to anonymous functions, we typically want the first named type
                |> Seq.tryHead
                |> Option.defaultValue "")
            let memberName = defaultArg memberName ""
            x.SetTag("code.function.name", $"{nameSpace}.{memberName}" ) |> ignore
          if x.GetTagItem "code.filepath" = null then x.SetTag("code.filepath", defaultArg path "") |> ignore
          if x.GetTagItem "code.lineno" = null then x.SetTag("code.lineno", defaultArg line 0) |> ignore

  module internal Activity =
    let inline isNotNull x = isNull x |> not

    let inline setStatus (status : ActivityStatusCode) (span : Activity) =
      if isNotNull span then
        span.SetStatus status |> ignore

    let inline setExn (e : exn) (span : Activity) =
      if isNotNull span|> not then
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
      if isNotNull span then
        setExn e span
        span  |> setStatus ActivityStatusCode.Error

    let setSourceLocation (sourceLoc : SourceLocation) (span : Activity) =
      if isNotNull span && sourceLoc <> SourceLocation.empty then
        span.SetTag("code.lineno", sourceLoc.lineNumber) |> ignore
        span.SetTag("code.filepath", sourceLoc.sourcePath) |> ignore

    let inline addOutcome (result : TestResult) (span : Activity) =
      if isNotNull span then
        let status = match result with
                     | Passed -> "Passed"
                     | Ignored _ -> "Ignored"
                     | Failed _ -> "Failed"
                     | Error _ -> "Error"
        span.SetTag("test.result.status", status) |> ignore
        span.SetTag("test.result.message", result) |> ignore

    let inline start (span : Activity) =
      if isNotNull span then
        span.Start() |> ignore
      span

    let inline stop (span : Activity) =
      if isNotNull span then
        span.Stop() |> ignore

    let inline setEndTimeNow (span : Activity) =
      if isNotNull span then
        span.SetEndTime DateTime.UtcNow |> ignore

    let inline createActivity (name : string) (source : ActivitySource) =
      if isNotNull source then
        source.CreateActivity(name, ActivityKind.Internal)
      else
        null

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
    let inline handleSuccess span =
      setEndTimeNow span
      addOutcome Passed span
      setStatus ActivityStatusCode.Ok span
    let inline handleFailure span e =
      setEndTimeNow span
      addExceptionOutcomeToSpan span e
      reraiseAnywhere e

    match test with
    | Sync test ->
      TestCode.Sync (fun () ->
        use span = start span
        try
          test ()
          handleSuccess span
        with
        | e ->
          handleFailure span e
      )

    | Async test ->
      TestCode.Async (async {
        use span = start span
        try
          do! test
          handleSuccess span
        with
        | e ->
          handleFailure span e
      })
    | AsyncFsCheck (testConfig, stressConfig, test) ->
      TestCode.AsyncFsCheck (testConfig, stressConfig, fun fsCheckConfig -> async {
        use span = start span
        try
          do! test fsCheckConfig
          handleSuccess span
        with
        | e ->
          handleFailure span e
      })
    | SyncWithCancel test->
      TestCode.SyncWithCancel (fun ct ->
        use span = start span
        try
          test ct
          handleSuccess span
        with
        | e ->
          handleFailure span e
      )

  /// <summary>Wraps each test with an OpenTelemetry Span/System.Diagnostics.Activity.</summary>
  /// <param name="config">ExpectoConfig</param>
  /// <param name="activitySource">Provides APIs start OpenTelemetry Span/System.Diagnostics.Activity</param>
  /// <param name="rootTest">The tests to wrap in span/activity.</param>
  /// <returns>Tests wrapped in a Span/Activity</returns>
  let addOpenTelemetry_SpanPerTest (config: ExpectoConfig) (activitySource: ActivitySource) (rootTest: Test) : Test =
    rootTest
    |> Test.toTestCodeList
    |> List.map (fun test ->
      let span = activitySource |> createActivity (config.joinWith.format test.name)
      span |> setSourceLocation (config.locate test.test)
      {test with test = wrapCodeWithSpan span test.test}
    )
    |> Test.fromFlatTests config.joinWith.asString

