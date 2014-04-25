namespace Fuchu

open System
open global.PerfUtil

[<AutoOpen; CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module FuchuFsCheck =
    open Fuchu
    open Fuchu.Helpers
    open Fuchu.Impl

    let testPerfCompareWithConfig config name =
        ()

    let testPerfCompare config name =
        ()

    let testPerfHistoryWithConfig config name =
        ()

    let testPerfHistory name =
        ()

type TODO = int

type FsCheck =
    static member PerfCompare(name, _ : TODO) =
        ()

(* EXAMPLES:


    type ``Serializer Comparison`` () =
        inherit NUnitPerf<ISerializer>()

        let fsp = testSerializer :> ISerializer
        let bfs = new BinaryFormatterSerializer() :> ISerializer
        let ndc = new NetDataContractSerializer() :> ISerializer
        let jdn = new JsonDotNetSerializer() :> ISerializer
        let pbn = new ProtoBufSerializer() :> ISerializer
        let ssj = new ServiceStackJsonSerializer() :> ISerializer
        let sst = new ServiceStackTypeSerializer() :> ISerializer

        let comparer = new MeanComparer(spaceFactor = 0.2, leastAcceptableImprovementFactor = 1.)

        let tester = new ImplemantationComparer<_>(fsp, [bfs;ndc;jdn;pbn;ssj;sst], throwOnError = true, comparer = comparer)
        let tests = PerfTest.OfModuleMarker<PerformanceTests.Marker> ()

        override __.PerfTester = tester :> _
        override __.PerfTests = tests


    type ``Past FsPickler Versions Comparison`` () =
        inherit NUnitPerf<ISerializer> ()

        let persistResults = true
        let persistenceFile = "fspPerf.xml"

        let fsp = testSerializer :> ISerializer
        let version = typeof<FsPickler>.Assembly.GetName().Version
        let comparer = new MeanComparer(spaceFactor = 0.2, leastAcceptableImprovementFactor = 0.8)
        let tests = PerfTest.OfModuleMarker<PerformanceTests.Marker> ()
        let tester = 
            new PastImplementationComparer<ISerializer>(
                fsp, version, historyFile = persistenceFile, throwOnError = true, comparer = comparer)

        override __.PerfTester = tester :> _
        override __.PerfTests = tests

        [<TestFixtureTearDown>]
        member __.Persist() =
            if persistResults then tester.PersistCurrentResults ()
*)