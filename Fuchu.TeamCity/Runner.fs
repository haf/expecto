module Fuchu.TeamCity

open Fuchu.Impl
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
    let withWriter =
        let locker = obj()
        fun name f ->
            let writer = getWriter name
            lock locker (fun () -> f writer)

    let beforeRun name = 
        withWriter name <| fun writer -> writer.OpenTest()
    let printPassed name time = 
        withWriter name <| fun writer -> writer.WriteDuration time
    let printIgnored name reason = 
        withWriter name <| fun writer -> writer.WriteIgnored reason
    let printFailed name error time =
        withWriter name <| fun writer ->
            writer.WriteDuration time
            writer.WriteFailed(error, error)
    let printException name (ex: #exn) time =
        withWriter name <| fun writer ->
            writer.WriteDuration time
            writer.WriteFailed(ex.Message, ex.ToString())
    let printers = 
        { TestPrinters.BeforeRun = beforeRun
          Passed = printPassed
          Ignored = printIgnored
          Failed = printFailed
          Exception = printException }
    eval printers pmap test

let run = runEval evalTeamCity