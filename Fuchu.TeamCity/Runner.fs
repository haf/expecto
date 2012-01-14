module Fuchu.TeamCity

open System
open JetBrains.TeamCity.ServiceMessages.Write
open JetBrains.TeamCity.ServiceMessages.Write.Special
open JetBrains.TeamCity.ServiceMessages.Write.Special.Impl
open JetBrains.TeamCity.ServiceMessages.Write.Special.Impl.Updater
open JetBrains.TeamCity.ServiceMessages.Write.Special.Impl.Writer

let formatter = ServiceMessageFormatter()

let getWriter name =
    let msgWriter = SpecializedServiceMessagesWriter(formatter, ResizeArray([ FlowMessageUpdater() :> IServiceMessageUpdater ]), fun _ -> ())
    new TeamCityTestWriter(msgWriter, name)

let evalTeamCity (test: Test) : TestRunResult list =
    let locker = obj()
    let beforeRun name = 
        let writer = getWriter name
        lock locker (fun () -> writer.OpenTest())
    let printPassed name time = 
        use writer = getWriter name
        lock locker (fun () -> writer.WriteDuration time)
    let printIgnored name reason = 
        use writer = getWriter name
        lock locker (fun () -> writer.WriteIgnored reason)
    let printFailed name error time =
        use writer = getWriter name
        lock locker (fun () -> 
                        writer.WriteDuration time
                        writer.WriteFailed(error, error))
    let printException name (ex: #exn) time =
        use writer = getWriter name
        lock locker (fun () -> 
                        writer.WriteDuration time
                        writer.WriteFailed(ex.Message, ex.ToString()))
    eval ignore printPassed printIgnored printFailed printException pmap test

let run = runEval evalTeamCity