module CodeLocationSamples 
open Expecto

let samplesFilePath = System.IO.Path.Combine(__SOURCE_DIRECTORY__, __SOURCE_FILE__)

type SampleTags = 
    | OriginalSupport
    | Pending
    | Focused
    | Async
    | Task

type LocationTestExample = {
    Name: string
    Test : Test
    ExpectedLocations: SourceLocation array
    Tags: SampleTags list
}


let testCaseExamples : LocationTestExample list = [
    {
        Name = nameof(testCase)
        ExpectedLocations = [| { sourcePath = samplesFilePath; lineNumber = (int __LINE__) + 1 } |]
        Test = testCase "testCase" <| (fun () -> ())
        Tags = [OriginalSupport]
    }
    {
        Name = nameof(ptestCase)
        ExpectedLocations = [| { sourcePath = samplesFilePath; lineNumber = (int __LINE__) + 1 } |]
        Test = ptestCase "ptestCaseExample" <| (fun () -> ())
        Tags = [OriginalSupport; Pending]
    }
    {
        Name = nameof(ftestCase)
        ExpectedLocations = [| { sourcePath = samplesFilePath; lineNumber = (int __LINE__) + 1 } |]
        Test = ftestCase "ftestCaseExample" <| (fun () -> ())
        Tags = [OriginalSupport; Focused]
    }
    {
        Name = nameof(testCaseAsync)
        ExpectedLocations = [| { sourcePath = samplesFilePath; lineNumber = (int __LINE__) + 1 } |]
        Test = testCaseAsync "testCaseAsyncExample" <| async { ignore ()}
        Tags = [OriginalSupport; Async]
    }
    {
        Name = nameof(testCaseTask)
        ExpectedLocations = [| { sourcePath = samplesFilePath; lineNumber = (int __LINE__) + 1 } |]
        Test = testCaseTask "testCaseTaskExample" <| fun () -> task { ignore ()}
        Tags = [OriginalSupport; Task]
    }
] 

let testBuilderExamples : LocationTestExample list = [
    {
        Name = nameof(test)
        ExpectedLocations = [| { sourcePath = samplesFilePath; lineNumber = (int __LINE__) + 1 } |]
        Test = test "Locate a builder" { ignore () }
        Tags = []
    }
    {
        Name = nameof(ptest)
        ExpectedLocations = [| { sourcePath = samplesFilePath; lineNumber = (int __LINE__) + 1 } |]
        Test = ptest "ptest" { ignore () }
        Tags = [Pending]
    }
    {
        Name = nameof(ftest)
        ExpectedLocations = [| { sourcePath = samplesFilePath; lineNumber = (int __LINE__) + 1 } |]
        Test = ftest "ftest" { ignore () }
        Tags = [Focused]
    }
    {
        Name = nameof(testAsync)
        ExpectedLocations = [| { sourcePath = samplesFilePath; lineNumber = (int __LINE__) + 1 } |]
        Test = testAsync "testAsyncExample" { ignore ()}
        Tags = [Async]
    }
    {
        Name = nameof(testTask)
        ExpectedLocations = [| { sourcePath = samplesFilePath; lineNumber = (int __LINE__) + 1 } |]
        Test = testTask "testTaskExample" { ignore ()}
        Tags = [Task]
    }
] 
