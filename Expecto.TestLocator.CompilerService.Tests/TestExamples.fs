module CodeLocationSamples 
open Expecto

let sampleFilePath = System.IO.Path.Combine(__SOURCE_DIRECTORY__, __SOURCE_FILE__)

let testCaseExampleLocation : SourceLocation = { sourcePath = sampleFilePath; lineNumber = __LINE__ |> int |> (+) 1 }
let testCaseExample = testCase "testCase" <| (fun () -> ())
let ptestCaseExampleLocation : SourceLocation = { sourcePath = sampleFilePath; lineNumber = __LINE__ |> int |> (+) 1 }
let ptestCaseExample = ptestCase "ptestCaseExample" <| (fun () -> ())
let ftestCaseExampleLocation : SourceLocation = { sourcePath = sampleFilePath; lineNumber = __LINE__ |> int |> (+) 1 }
let ftestCaseExample = ftestCase "ftestCaseExample" <| (fun () -> ())

let testBuilderExampleLocation : SourceLocation = { sourcePath = sampleFilePath; lineNumber = __LINE__ |> int |> (+) 1 }
let testBuilderExample = test "Locate a builder" { ignore () }
let ptestBuilderExampleLocation : SourceLocation = { sourcePath = sampleFilePath; lineNumber = __LINE__ |> int |> (+) 1 }
let ptestBuilderExample = ptest "Locate a builder" { ignore () }
let ftestBuilderExampleLocation : SourceLocation = { sourcePath = sampleFilePath; lineNumber = __LINE__ |> int |> (+) 1 }
let ftestBuilderExample = ftest "Locate a builder" { ignore () }

let testParamExamplesLocation : SourceLocation = { sourcePath = sampleFilePath; lineNumber = __LINE__ |> int |> (+) 1 }
let testParamExamples = List.ofSeq (testParam 5 [
        "t1", (fun _ () -> ())
        "t2", (fun _ () -> ())
    ])

let testCaseAsyncExampleLocation : SourceLocation = { sourcePath = sampleFilePath; lineNumber = __LINE__ |> int |> (+) 1 }
let testCaseAsyncExample = testCaseAsync "testCaseExample" <| async { ignore ()}

let testCaseTheoryExaLocation : SourceLocation = { sourcePath = sampleFilePath; lineNumber = __LINE__ |> int |> (+) 1 }
let testTheoryExample = testTheory "testTheory example" [1;2] (fun _ -> ())

let testListExampleLocation : SourceLocation = { sourcePath = sampleFilePath; lineNumber = __LINE__ |> int |> (+) 1 }
let testListExample = testList "testList example" []
