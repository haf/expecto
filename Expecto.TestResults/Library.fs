module Expecto.TestResults

open System.IO
open System.Xml.Linq

let writeNUnitSummary (file, assemblyName) (summary: Impl.TestRunSummary) =
    // v3: https://github.com/nunit/docs/wiki/Test-Result-XML-Format
    // this impl is v2: http://nunit.org/docs/files/TestResult.xml
    let totalTests = summary.errored @ summary.failed @ summary.ignored @ summary.passed
    let testCaseElements =
        totalTests
        |> Seq.map (fun (flatTest, test) ->
            let content =
                match test.result with
                | Impl.TestResult.Passed ->
                  [|
                    XAttribute(XName.Get "executed", "True") |> box
                    XAttribute(XName.Get "result", "Success") |> box
                    XAttribute(XName.Get "success", "True") |> box
                    XAttribute(XName.Get "time",
                        System.String.Format(System.Globalization.CultureInfo.InvariantCulture,
                            "{0:0.000}", test.duration.TotalSeconds)) |> box
                    XAttribute(XName.Get "asserts", "0") |> box
                  |]
                | Impl.TestResult.Error e ->
                  [|
                    XAttribute(XName.Get "executed", "True") |> box
                    XAttribute(XName.Get "result", "Failure") |> box
                    XAttribute(XName.Get "success", "False") |> box
                    XAttribute(XName.Get "time",
                        System.String.Format(System.Globalization.CultureInfo.InvariantCulture,
                            "{0:0.000}", test.duration.TotalSeconds)) |> box
                    XAttribute(XName.Get "asserts", "0") |> box
                    XElement(XName.Get "failure",
                      [|
                        XElement(XName.Get "message", e.Message)
                        XElement(XName.Get "stack-trace", e.ToString())
                      |]) |> box
                  |]
                | Impl.TestResult.Failed msg ->
                  [|
                    XAttribute(XName.Get "executed", "True") |> box
                    XAttribute(XName.Get "result", "Failure") |> box
                    XAttribute(XName.Get "success", "False") |> box
                    XAttribute(XName.Get "time",
                        System.String.Format(System.Globalization.CultureInfo.InvariantCulture,
                            "{0:0.000}", test.duration.TotalSeconds)) |> box
                    XAttribute(XName.Get "asserts", "0") |> box
                    XElement(XName.Get "failure",
                        XElement(XName.Get "message", msg)) |> box
                  |]
                | Impl.TestResult.Ignored msg ->
                  [|
                    XAttribute(XName.Get "executed", "False") |> box
                    XAttribute(XName.Get "result", "Ignored") |> box
                    XElement(XName.Get "reason",
                        XElement(XName.Get "message", msg)) |> box
                  |]

            XElement(XName.Get "test-case",
              [|
                yield XAttribute(XName.Get "name", flatTest.name) |> box
                yield! content
              |]))
    let element =
      XElement(
        XName.Get "test-results",
        [|
          let d = System.DateTime.Now
          yield XAttribute(XName.Get "date", d.ToString("yyyy-MM-dd")) |> box
          yield XAttribute(XName.Get "name", assemblyName) |> box
          yield XAttribute(XName.Get "total", totalTests.Length) |> box
          yield XAttribute(XName.Get "errors", summary.errored.Length) |> box
          yield XAttribute(XName.Get "failures", summary.failed.Length) |> box
          yield XAttribute(XName.Get "ignored", summary.ignored.Length) |> box
          yield XAttribute(XName.Get "not-run", "0") |> box
          yield XAttribute(XName.Get "inconclusive", "0") |> box
          yield XAttribute(XName.Get "skipped", "0") |> box
          yield XAttribute(XName.Get "invalid", "0") |> box
          //yield XAttribute(XName.Get "date", sprintf "0") |> box
          yield XAttribute(XName.Get "time", d.ToString("HH:mm:ss")) |> box
          yield XElement(XName.Get "environment",
            [|
              yield XAttribute(XName.Get "expecto-version", AssemblyInfo.AssemblyVersionInformation.AssemblyVersion) |> box
              yield XAttribute(XName.Get "clr-version", string System.Environment.Version) |> box
              yield XAttribute(XName.Get "os-version", System.Environment.OSVersion) |> box
              yield XAttribute(XName.Get "platform", "Linux") |> box
              yield XAttribute(XName.Get "cwd", System.Environment.CurrentDirectory) |> box
              yield XAttribute(XName.Get "machine-name", System.Environment.MachineName) |> box
              yield XAttribute(XName.Get "user", System.Environment.UserName) |> box
              yield XAttribute(XName.Get "user-domain", System.Environment.UserDomainName) |> box
            |]) |> box
          yield XElement(XName.Get "culture-info",
            [|
              yield XAttribute(XName.Get "current-culture", string System.Globalization.CultureInfo.CurrentCulture) |> box
              yield XAttribute(XName.Get "current-uiculture", string System.Globalization.CultureInfo.CurrentUICulture) |> box
            |]) |> box
          yield XElement(XName.Get "test-suite",
            [|
              yield XAttribute(XName.Get "type", "Assembly") |> box
              yield XAttribute(XName.Get "name", assemblyName) |> box
              yield XAttribute(XName.Get "executed", "True") |> box
              yield XAttribute(XName.Get "result", if summary.successful then "Success" else "Failure") |> box
              yield XAttribute(XName.Get "success", if summary.successful then "True" else "False") |> box
              yield XAttribute(XName.Get "time",
                  System.String.Format(System.Globalization.CultureInfo.InvariantCulture,
                              "{0:0.000}", summary.duration.TotalSeconds)) |> box
              yield XAttribute(XName.Get "asserts", "0") |> box
              yield XElement(XName.Get "results",
                [|
                  yield! testCaseElements
                |]) |> box
            |]) |> box
        |])

    let doc = XDocument([|element|])
    let path = Path.GetFullPath file
    doc.Save(path)

/// Generate a TestResults.xml file; after the original summary has printed.
let appendSummaryHandler handleSummary (def: Expecto.Impl.TestPrinters) =
    { def with
        summary = fun _config summary -> async {
            do! def.summary _config summary
            handleSummary summary
          }
      }