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
        | :? AssertException as e ->
          stop span
          // TODO: this message construction is fragile duplication
          let msg =
            "\n" + e.Message + "\n" +
            (e.StackTrace.Split('\n')
            |> Seq.skipWhile (fun l -> l.StartsWith("   at Expecto.Expect."))
            |> Seq.truncate 5
            |> String.concat "\n")
          let result = Failed msg
          addOutcome result span
          setExnMarkFailed e span
          reraiseAnywhere e
        | :? FailedException as e ->
          stop span
          let result = Failed ("\n"+e.Message)
          addOutcome result span
          setExnMarkFailed e span
          reraiseAnywhere e
        | :? IgnoreException as e ->
          stop span
          let result = Ignored e.Message
          addOutcome result span
          setExn e span
          reraiseAnywhere e
        | :? AggregateException as e when e.InnerExceptions.Count = 1 ->
          stop span
          if e.InnerException :? IgnoreException then
            let result = Ignored e.InnerException.Message
            addOutcome result span
            setExn e span
          else
            let result = Error e.InnerException
            addOutcome result span
            setExnMarkFailed e span
          reraiseAnywhere e
        | e ->
          stop span
          let result = Error e
          addOutcome result span
          setExnMarkFailed e span
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
        | :? AssertException as e ->
          stop span
          // TODO: this message construction is fragile duplication
          let msg =
            "\n" + e.Message + "\n" +
            (e.StackTrace.Split('\n')
            |> Seq.skipWhile (fun l -> l.StartsWith("   at Expecto.Expect."))
            |> Seq.truncate 5
            |> String.concat "\n")
          let result = Failed msg
          addOutcome result span
          setExnMarkFailed e span
          reraiseAnywhere e
        | :? FailedException as e ->
          stop span
          let result = Failed ("\n"+e.Message)
          addOutcome result span
          setExnMarkFailed e span
          reraiseAnywhere e
        | :? IgnoreException as e ->
          stop span
          let result = Ignored e.Message
          addOutcome result span
          setExn e span
          reraiseAnywhere e
        | :? AggregateException as e when e.InnerExceptions.Count = 1 ->
          stop span
          if e.InnerException :? IgnoreException then
            let result = Ignored e.InnerException.Message
            addOutcome result span
            setExn e span
          else
            let result = Error e.InnerException
            addOutcome result span
            setExnMarkFailed e span
          reraiseAnywhere e
        | e ->
          stop span
          let result = Error e
          addOutcome result span
          setExnMarkFailed e span
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
        | :? AssertException as e ->
          stop span
          // TODO: this message construction is fragile duplication
          let msg =
            "\n" + e.Message + "\n" +
            (e.StackTrace.Split('\n')
            |> Seq.skipWhile (fun l -> l.StartsWith("   at Expecto.Expect."))
            |> Seq.truncate 5
            |> String.concat "\n")
          let result = Failed msg
          addOutcome result span
          setExnMarkFailed e span
          reraiseAnywhere e
        | :? FailedException as e ->
          stop span
          let result = Failed ("\n"+e.Message)
          addOutcome result span
          setExnMarkFailed e span
          reraiseAnywhere e
        | :? IgnoreException as e ->
          stop span
          let result = Ignored e.Message
          addOutcome result span
          setExn e span
          reraiseAnywhere e
        | :? AggregateException as e when e.InnerExceptions.Count = 1 ->
          stop span
          if e.InnerException :? IgnoreException then
            let result = Ignored e.InnerException.Message
            addOutcome result span
            setExn e span
          else
            let result = Error e.InnerException
            addOutcome result span
            setExnMarkFailed e span
          reraiseAnywhere e
        | e ->
          stop span
          let result = Error e
          addOutcome result span
          setExnMarkFailed e span
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
        | :? AssertException as e ->
          stop span
          // TODO: this message construction is fragile duplication
          let msg =
            "\n" + e.Message + "\n" +
            (e.StackTrace.Split('\n')
            |> Seq.skipWhile (fun l -> l.StartsWith("   at Expecto.Expect."))
            |> Seq.truncate 5
            |> String.concat "\n")
          let result = Failed msg
          addOutcome result span
          setExnMarkFailed e span
          reraiseAnywhere e
        | :? FailedException as e ->
          stop span
          let result = Failed ("\n"+e.Message)
          addOutcome result span
          setExnMarkFailed e span
          reraiseAnywhere e
        | :? IgnoreException as e ->
          stop span
          let result = Ignored e.Message
          addOutcome result span
          setExn e span
          reraiseAnywhere e
        | :? AggregateException as e when e.InnerExceptions.Count = 1 ->
          stop span
          if e.InnerException :? IgnoreException then
            let result = Ignored e.InnerException.Message
            addOutcome result span
            setExn e span
          else
            let result = Error e.InnerException
            addOutcome result span
            setExnMarkFailed e span
          reraiseAnywhere e
        | e ->
          stop span
          let result = Error e
          addOutcome result span
          setExnMarkFailed e span
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
