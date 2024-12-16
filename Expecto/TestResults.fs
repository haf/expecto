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
      | PassedWithMessage _
      | Passed -> "Success"
      | Error _
      | Failed _ -> "Failure"
      | Ignored _ -> "Ignored"
      |> addAttribute "result"

      match test.result with
      | PassedWithMessage _
      | Passed -> addAttribute "success" "True"
      | Error _
      | Failed _ -> addAttribute "success" "False"
      // Ignored tests are neither successful nor failed.
      | Ignored _ -> ()

      String.Format(CultureInfo.InvariantCulture, "{0:0.000}", test.duration.TotalSeconds)
      |> addAttribute "time"

      // TODO: implement it.
      addAttribute "asserts" "0"

      let passNode = XElement(XName.Get "pass")
      let failureNode = XElement(XName.Get "failure")

      // Some more details that explain why a test was not executed.
      match test.result with
      | Passed -> ()
      | PassedWithMessage m ->
        passNode.Add(XName.Get "message", XCData m)
        element.Add passNode
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
  let xAttr name data = XAttribute(XName.Get name, data)
  let element =
    XElement(XName.Get "test-results",
      xAttr "date" (d.ToString("yyyy-MM-dd")),
      xAttr "name" assemblyName,
      xAttr "total" totalTests.Length,
      xAttr "errors" summary.errored.Length,
      xAttr "failures" summary.failed.Length,
      xAttr "ignored" summary.ignored.Length,
      xAttr "not-run" "0",
      xAttr "inconclusive" "0",
      xAttr "skipped" "0",
      xAttr "invalid" "0",
      xAttr "time" (d.ToString("HH:mm:ss")),
      XElement(XName.Get "environment",
        xAttr "expecto-version" expectoVersion,
        xAttr "clr-version" Environment.Version,
        xAttr "os-version" Environment.OSVersion.VersionString,
        xAttr "platform" Environment.OSVersion.Platform,
        xAttr "cwd" Environment.CurrentDirectory,
        xAttr "machine-name" Environment.MachineName,
        xAttr "user" Environment.UserName,
        xAttr "user-domain" Environment.UserDomainName
      ),
      XElement(XName.Get "culture-info",
        xAttr "current-culture", CultureInfo.CurrentCulture,
        xAttr "current-uiculture", CultureInfo.CurrentUICulture
      ),
      XElement(XName.Get "test-suite",
        xAttr "type" "Assembly",
        xAttr "name" assemblyName,
        xAttr "executed" "True",
        xAttr "result" (if summary.successful then "Success" else "Failure"),
        xAttr "success" (if summary.successful then "True" else "False"),
        xAttr "time"
          (String.Format(CultureInfo.InvariantCulture,
            "{0:0.000}", summary.duration.TotalSeconds)),
        xAttr "asserts" "0",
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
        | PassedWithMessage msg -> [|makeMessageNode "pass" msg|]
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
