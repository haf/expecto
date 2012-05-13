namespace Fuchu.TDNet

open TestDriven.Framework
open Fuchu

type FuchuRunner() =
    let locker = obj()
    let flock x = lock locker x
    let onPassed (listener: ITestListener) name time = 
        flock
            (fun () -> 
                let result = TestResult(Name = name, 
                                        State = TestState.Passed,
                                        TimeSpan = time)
                listener.TestFinished result)

    let onIgnored (listener: ITestListener) name reason = 
        flock
            (fun () ->
                let result = TestResult(Name = name,
                                        State = TestState.Ignored,
                                        Message = reason)
                listener.TestFinished result)

    let onFailed (listener: ITestListener) name error time = 
        flock
            (fun () -> 
                let result = TestResult(Name = name, 
                                        State = TestState.Failed, 
                                        Message = error,
                                        TimeSpan = time)
                listener.TestFinished result)

    let onException (listener: ITestListener) name ex time =
        flock
            (fun () -> 
                let result = TestResult(Name = name, 
                                        State = TestState.Failed, 
                                        Message = ex.ToString(), 
                                        StackTrace = ex.ToString(),
                                        TimeSpan = time)
                listener.TestFinished result)

    let run listener = 
        eval ignore (onPassed listener) (onIgnored listener) (onFailed listener) (onException listener) pmap

    let resultCountsToTDNetResult (c: TestResultCounts) =
        match c.Passed, c.Failed, c.Errored with
        | _, _, x when x > 0 -> TestRunState.Error
        | _, x, _ when x > 0 -> TestRunState.Failure
        | 0, 0, 0            -> TestRunState.NoTests
        | _                  -> TestRunState.Success

    let runAndSummary listener test =
        run listener test |> sumTestResults |> resultCountsToTDNetResult

    interface ITestRunner with
        member x.RunAssembly(listener, assembly) = 
            let test = Test.FromAssembly assembly
            runAndSummary listener test
        member x.RunMember(listener, assembly, metod) = 
            let test = Test.FromMember metod
            runAndSummary listener test
        member x.RunNamespace(listener, assembly, ns) =
            let test = 
                assembly.GetExportedTypes()
                |> Seq.filter (fun t -> t.Namespace = ns)
                |> Seq.map Test.FromType
                |> Seq.toList
                |> TestList
                |> withLabel (assembly.FullName.Split ',').[0]
            runAndSummary listener test

