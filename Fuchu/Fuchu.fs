module Fuchu

open System
open FSharpx

type TestCode = unit -> Choice<unit, string>

type Test = 
    | TestCase of TestCode
    | TestList of Test list
    | TestLabel of string * Test

let withLabel lbl t = TestLabel (lbl, t)

type TestResult = 
    | Passed
    | Failed of string
    | Exception of exn

let testResultToString =
    function
    | Passed -> "Passed"
    | Failed error -> "Failed: " + error
    | Exception e -> "Exception: " + e.ToString()

type TestResults = (string * TestResult) list

let testResultsToString (results: TestResults) =
    results 
    |> Seq.map (fun (n,t) -> sprintf "%s: %s" n (testResultToString t))
    |> String.concat "\n"

type TestResultCounts = {
    Passed: int
    Failed: int
    Errored: int
}

let sumTestResults (results: TestResults) =
    let counts = 
        results 
        |> Seq.map snd
        |> Seq.countBy (function
                        | Passed -> 0
                        | Failed _ -> 1
                        | Exception _ -> 2)
        |> dict
    let get i = 
        counts |> Dictionary.tryFind i |> Option.getOrDefault
    { Passed = get 0
      Failed = get 1
      Errored = get 2 }

let ok = Choice1Of2 ()
let fail (msg: string) = Choice2Of2 msg
let failf fmt = Printf.ksprintf Choice2Of2 fmt

let assertEqual a b = 
    if a = b 
        then ok
        else failf "Expected %A = %A" a b

let exec =
    let rec loop (parentName: string) (partialResults: TestResults) =
        function
        | TestLabel (name, test) -> loop (parentName + "/" + name) partialResults test
        | TestCase test -> 
            try
                match test() with
                | Choice1Of2() -> (parentName, Passed)::partialResults
                | Choice2Of2 error -> (parentName, Failed error)::partialResults
            with e -> (parentName, Exception e)::partialResults
        | TestList tests -> List.collect (loop parentName partialResults) tests

    loop "" []

let run tests = 
    let results = exec tests
    let report = testResultsToString results
    let summary = sumTestResults results
    printfn "%s" report
    printfn "%d tests run: %d passed, %d failed, %d errored"
        (summary.Errored + summary.Failed + summary.Passed)
        summary.Passed
        summary.Failed
        summary.Errored

