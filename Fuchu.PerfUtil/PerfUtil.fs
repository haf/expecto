namespace Fuchu

open System
open global.PerfUtil

module PerfUtilFac =
    let todo () = ()

[<AutoOpen; CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module FuchuPerfUtil =
    open Fuchu
    open Fuchu.Helpers
    open Fuchu.Impl

    open System.IO

    type PerfConfig =
        { a : string }

    type CompareImplsConfig =
        { throwOnError : bool
          comparer     : IPerformanceComparer
          verbose      : bool }
        static member Defaults =
            { throwOnError = true
              comparer     = MeanComparer()
              verbose      = true }

    /// A configuration for the historical performance development
    /// for a given thing.
    type CompareHistoryConfig =
          /// path to history file
        { historyFile  : string
          comparer     : IPerformanceComparer
          verbose      : bool
          throwOnError : bool
          overwrite    : bool }
        /// Defaults to a xml file in the currently executing DLL's directory
        /// named the same as the collection of perf tests.
        static member Defaults testName =
            { historyFile  = Path.Combine(Path.GetDirectoryName(PerfUtil.DefaultPersistenceFile), testName + ".xml")
              comparer     = MeanComparer()
              verbose      = true
              throwOnError = true
              overwrite    = true }

    /// Create a new performance test. The values given from this method are usable both
    /// by PerfUtil as well as Fuchu's testPerfCompareWithConfig, testPerfCompare,
    /// testPerfHistoryWithConfig and testPerfHistory. You can give the values from this
    /// function to both Fuchu and PerfUtil.
    let perfTest name (testImpl : 'a -> unit when 'a :> ITestable) =
        { PerfTest.Id = name
          Test        = testImpl }

    let testPerfCompareWithConfig (conf : CompareImplsConfig) name (subject : 'a) (alternatives : 'a list) (tests : PerfTest<'a> list) =
        let tester () =
            new ImplemantationComparer<_>(subject, alternatives, conf.comparer, conf.verbose, conf.throwOnError)
                :> PerformanceTester<'a>

        testCase name <| fun _ ->
            let results = PerfTest.run tester tests
            // TODO: handle saving or displaying of results
            // TODO: handle by saving artifacts? Chart them and save charts?
            ()

    let testPerfCompare<'a when 'a :> ITestable> name (subj : 'a) (alts : 'a list) (tests : 'a PerfTest list) =
        testPerfCompareWithConfig CompareImplsConfig.Defaults name subj alts tests

    let testPerfHistoryWithConfig (config : CompareHistoryConfig) name (subject : 'a) (testRunId : string) (tests : 'a PerfTest list) =
        let tester =
            new PastImplementationComparer<_>(
                subject, testRunId, config.historyFile, config.comparer,
                config.verbose, config.throwOnError, config.overwrite)

        testCase name <| fun _ ->
            let results = PerfTest.run (fun () -> tester :> PerformanceTester<'a>) tests
            tester.PersistCurrentResults()

    let testPerfHistory name (subject : 'a) (testRunId : string) =
        testPerfHistoryWithConfig (CompareHistoryConfig.Defaults name) name subject testRunId

module Usage =
    open global.PerfUtil

    module Types =
        type Y = { a : string; b : int }
        type X = { c : Nullable<int>; d : uint64 }

    type Serialiser =
        inherit ITestable
        abstract member Serialise<'a> : 'a -> unit

    type MySlowSerialiser() =
        interface ITestable with
            member x.Name = "Slow Serialiser"
        interface Serialiser with
            member x.Serialise _ =
                System.Threading.Thread.Sleep(30)

    type FastSerialiser() =
        interface ITestable with
            member x.Name = "Fast Serialiser"
        interface Serialiser with
            member x.Serialise _ =
                System.Threading.Thread.Sleep(10)

    type FastSerialiserAlt() =
        interface ITestable with
            member x.Name = "Fast Serialiser Alt"
        interface Serialiser with
            member x.Serialise _ =
                System.Threading.Thread.Sleep(20)

    let alts : Serialiser list = [ FastSerialiser(); FastSerialiserAlt() ]
    let subj = MySlowSerialiser() :> Serialiser

    open Types

    let normal_serlialisation : PerfTest<Serialiser> list = [
        perfTest "serialising string" <| fun s ->
            s.Serialise("wowowow")
        perfTest "serialising record" <| fun s ->
            s.Serialise { a = "hello world"; b = 42 }
        ]

    [<Tests>]
    let tests =
        testList "performance comparison tests" [
            testPerfCompare "implementations of Serialiser" subj alts normal_serlialisation
            testPerfHistory "historical MySlowSerialiser" subj "v1.2.3" normal_serlialisation
        ]

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