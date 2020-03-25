module Expecto.TestResults

open System
open System.Globalization
open System.IO
open System.Reflection
open System.Xml.Linq
open System.Xml
open Impl

let private assemblyName = Assembly.GetEntryAssembly().GetName().Name

let private xmlSave fileName (doc: XDocument) =
  let path = Path.GetFullPath fileName
  Path.GetDirectoryName path
  |> Directory.CreateDirectory
  |> ignore
  let settings = XmlWriterSettings(CheckCharacters = false)
  use writer = XmlWriter.Create(path, settings)
  doc.Save writer

/// Generate test results using NUnit v2 schema.
let writeNUnitSummary file (summary: TestRunSummary) =
  // v3: https://github.com/nunit/docs/wiki/Test-Result-XML-Format
  // this impl is v2: http://nunit.org/docs/files/TestResult.xml
  let totalTests = summary.errored @ summary.failed @ summary.ignored @ summary.passed
  let testCaseElements =
    totalTests
    |> Seq.sortByDescending (fun (_,test) -> test.result.order,test.duration.TotalSeconds)
    |> Seq.map (fun (flatTest, test) ->
      let element =
        XElement(XName.Get "test-case",
          XAttribute(XName.Get "name", flatTest.name))
      let addAttribute name (content: string) =
        element.Add(XAttribute(XName.Get name, content))

      match test.result with
      | Ignored _ -> "False"
      | _ -> "True"
      |> addAttribute "executed"

      match test.result with
      | Passed -> "Success"
      | Error _
      | Failed _ -> "Failure"
      | Ignored _ -> "Ignored"
      |> addAttribute "result"

      match test.result with
      | Passed -> addAttribute "success" "True"
      | Error _
      | Failed _ -> addAttribute "success" "False"
      // Ignored tests are neither successful nor failed.
      | Ignored _ -> ()

      String.Format(CultureInfo.InvariantCulture, "{0:0.000}", test.duration.TotalSeconds)
      |> addAttribute "time"

      // TODO: implement it.
      addAttribute "asserts" "0"

      let failureNode = XElement(XName.Get "failure")

      // Some more details that explain why a test was not executed.
      match test.result with
      | Passed -> ()
      | Error e ->
        failureNode.Add(XName.Get "message", XCData e.Message)
        failureNode.Add(XName.Get "stack-trace", XCData e.StackTrace)
        element.Add failureNode
      | Failed msg ->
        failureNode.Add(XName.Get "message", XCData msg)
        element.Add failureNode
      | Ignored msg -> element.Add(XElement(XName.Get "reason", XElement(XName.Get "message", XCData msg)))
      element)
  let d = DateTime.Now
  let element =
    XElement(
      XName.Get "test-results",
      XAttribute(XName.Get "date", d.ToString("yyyy-MM-dd")),
      XAttribute(XName.Get "name", assemblyName),
      XAttribute(XName.Get "total", totalTests.Length),
      XAttribute(XName.Get "errors", summary.errored.Length),
      XAttribute(XName.Get "failures", summary.failed.Length),
      XAttribute(XName.Get "ignored", summary.ignored.Length),
      XAttribute(XName.Get "not-run", "0"),
      XAttribute(XName.Get "inconclusive", "0"),
      XAttribute(XName.Get "skipped", "0"),
      XAttribute(XName.Get "invalid", "0"),
      XAttribute(XName.Get "time", d.ToString("HH:mm:ss")),
      XElement(XName.Get "environment",
        XAttribute(XName.Get "expecto-version", AssemblyInfo.AssemblyVersionInformation.AssemblyVersion),
        XAttribute(XName.Get "clr-version", string Environment.Version),
        XAttribute(XName.Get "os-version", Environment.OSVersion.VersionString),
        XAttribute(XName.Get "platform", Environment.OSVersion.Platform),
        XAttribute(XName.Get "cwd", Environment.CurrentDirectory),
        XAttribute(XName.Get "machine-name", Environment.MachineName),
        XAttribute(XName.Get "user", Environment.UserName),
        XAttribute(XName.Get "user-domain", Environment.UserDomainName)
      ),
      XElement(XName.Get "culture-info",
        XAttribute(XName.Get "current-culture", string CultureInfo.CurrentCulture),
        XAttribute(XName.Get "current-uiculture", string CultureInfo.CurrentUICulture)
      ),
      XElement(XName.Get "test-suite",
        XAttribute(XName.Get "type", "Assembly"),
        XAttribute(XName.Get "name", assemblyName),
        XAttribute(XName.Get "executed", "True"),
        XAttribute(XName.Get "result", if summary.successful then "Success" else "Failure"),
        XAttribute(XName.Get "success", if summary.successful then "True" else "False"),
        XAttribute(XName.Get "time",
          String.Format(CultureInfo.InvariantCulture,
            "{0:0.000}", summary.duration.TotalSeconds)),
        XAttribute(XName.Get "asserts", "0"),
        XElement(XName.Get "results", testCaseElements)
        )
      )

  element
  |> XDocument
  |> xmlSave file

let writeJUnitSummary file (summary: Impl.TestRunSummary) =

  // junit does not have an official xml spec
  // this is a minimal implementation to get gitlab to recognize the tests:
  // https://docs.gitlab.com/ee/ci/junit_test_reports.html
  let totalTests = summary.errored @ summary.failed @ summary.ignored @ summary.passed
  let testCaseElements =
    totalTests
    |> Seq.sortByDescending (fun (_,test) -> test.result.order,test.duration.TotalSeconds)
    |> Seq.map (fun (flatTest, test) ->
      let content: XObject[] =
        let makeMessageNode messageType (message: string) =
          XElement(XName.Get messageType,
            XAttribute(XName.Get "message", message))
        match test.result with
        | Passed -> [||]
        | Error e ->
          let message = makeMessageNode "error" e.Message
          message.Add(XCData(e.ToString()))
          [|message|]
        | Failed msg -> [|makeMessageNode "failure" msg|]
        | Ignored msg -> [|makeMessageNode "skipped" msg|]

      XElement(XName.Get "testcase",
        [|
          yield XAttribute(XName.Get "name", flatTest.name) :> XObject
          yield XAttribute(XName.Get "time",
            System.String.Format(CultureInfo.InvariantCulture,
              "{0:0.000}", test.duration.TotalSeconds)) :> XObject
          yield! content
        |]) :> XObject)
  let element =
    XElement(
      XName.Get "testsuites",
        XElement(XName.Get "testsuite",
          [|
            yield XAttribute(XName.Get "name", assemblyName) :> XObject
            yield! testCaseElements
          |])
      )

  element
  |> XDocument
  |> xmlSave file
