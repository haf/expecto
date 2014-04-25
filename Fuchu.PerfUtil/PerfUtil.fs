namespace Fuchu

open System
open global.PerfUtil

[<AutoOpen; CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module FuchuPerfUtil =
    open Fuchu
    open Fuchu.Helpers
    open Fuchu.Impl

    open System.IO

    /// Create a new performance test. The values given from this method are usable both
    /// by PerfUtil as well as Fuchu's testPerfImplsWithConfig, testPerfImpls,
    /// testPerfHistoryWithConfig and testPerfHistory. You can give the values from this
    /// function to both Fuchu and PerfUtil.
    let perfTest name testImpl =
        { PerfTest.Id = name
          Test        = testImpl }

    type PerfImplsConf =
          /// <summary>
          /// Whether to throw <see cref="PerfUtil.PerformanceException" />
          /// if the subject is slower than the alternative that it is compared to.
          /// Useful for making sure you don't accidentally write code that degrades
          /// performance. Defaults to false.
          /// </summary>
        { throwOnError  : bool
          /// <summary>
          /// The comparer for how much 'better' you need the subject to be. Defaults to
          /// <see cref="PerfUtil.MeanComparer" />.
          /// </summary>
          comparer      : IPerformanceComparer
          /// Whether to print results to stdout. Defaults to true.
          verbose       : bool
          /// An optional function that is called when the perf tests have been completed
          /// allowing you to extrace the results and save them or display them or show them
          /// to your mom.
          handleResults : TestSession list -> unit }
        static member Defaults =
            { throwOnError  = false
              comparer      = WeightedComparer()
              verbose       = true
              handleResults = fun _ -> () }

    /// <summary>
    /// Compares given implementation performance against a collection of other implementations.
    /// Use the 'perfTest' function to easily construct test cases.
    /// </summary>
    /// <param name="conf">The <see cref="" /> configuration</param>
    /// <param name="name">Name for the group of performance tests</param>
    /// <param name="subject">Implementation under test.</param>
    /// <param name="alternatives">Secondary implementations to be compared against.</param>
    /// <param name="tests">The performance tests to run against the subject and the alternatives.</param>
    let testPerfImplsWithConfig (conf : PerfImplsConf) name subject alternatives tests =
        let tester () =
            new ImplementationComparer<_>(subject, alternatives, conf.comparer, conf.verbose, conf.throwOnError)
                :> PerformanceTester<_>

        testCase name <| fun _ ->
            let results = PerfTest.run tester tests
            conf.handleResults results

    /// <summary>
    /// Compares given implementation performance against a collection of other implementations.
    /// Use the 'perfTest' function to easily construct test cases. With this function, the configuration
    /// will be the sane defaults; if you want to override them, please see <see cref="testPerfImplsWithConfig" />.
    /// </summary>
    /// <param name="name">Name for the group of performance tests</param>
    /// <param name="subject">Implementation under test.</param>
    /// <param name="alternatives">Secondary implementations to be compared against.</param>
    /// <param name="tests">The performance tests to run against the subject and the alternatives.</param>
    let testPerfImpls name subj alts tests =
        testPerfImplsWithConfig PerfImplsConf.Defaults name subj alts tests

    /// A configuration for the historical performance development for a given implementation.
    type PerfHistoryConf =
          /// path to history file
        { historyFile   : string
          comparer      : IPerformanceComparer
          /// Whether to print results to stdout. Defaults to true.
          verbose       : bool
          /// Whether to throw if the subject has gotten worse in comparison to previous runs, 
          /// as decided by the 'comparer'.
          throwOnError  : bool
          /// Whether to overwrite previous tests. Defaults to true.
          overwrite     : bool
          /// An optional function that is called when the perf tests have been completed
          /// allowing you to extrace the results and save them or display them or show them
          /// to your mom. It will be passed the path of the xml file with test results and
          /// the list of TestSessions that comes from PerfUtil.
          handleResults : string * TestSession list -> unit }
        /// Defaults to a xml file in the currently executing DLL's directory
        /// named the same as the collection of perf tests.
        static member Defaults testName =
            { historyFile   = Path.Combine(Path.GetDirectoryName(PerfUtil.DefaultPersistenceFile), testName + ".xml")
              comparer      = WeightedComparer(0.05, 1.0)
              verbose       = true
              throwOnError  = false
              overwrite     = true
              handleResults = fun _ -> () }

    /// <summary>
    /// Compares current implementation against a collection of past tests.
    /// </summary>
    /// <param name="conf">Configuration for the historical performance test</param>
    /// <param name="name">Name for the group of performance tests</param>
    /// <param name="subject">(Current) implementation under test.</param>
    /// <param name="testRunId">The id of the test run; this must be ticking upwards, so
    /// a recommended value for this parameter is the current assembly version. You can use
    /// for example https://github.com/Albacore/albacore/#docs-asmver together with
    /// https://github.com/Albacore/albacore/#versionizer and
    /// https://github.com/haf/semver to manage your versions in a CI-environment.</param>
    /// <param name="tests">The performance tests to run against the subject and the alternatives.</param>
    let testPerfHistoryWithConfig (conf : PerfHistoryConf) name subject (testRunId: string) tests =
        let tester =
            new PastImplementationComparer<_>(
                subject, testRunId, conf.historyFile, conf.comparer,
                conf.verbose, conf.throwOnError, conf.overwrite)

        testCase name <| fun _ ->
            let results = PerfTest.run (fun () -> tester :> PerformanceTester<_>) tests
            tester.PersistCurrentResults()
            conf.handleResults(conf.historyFile, results)

    /// <summary>
    /// Compares current implementation against a collection of past tests.
    /// </summary>
    /// <param name="name">Name for the group of performance tests</param>
    /// <param name="subject">(Current) implementation under test.</param>
    /// <param name="testRunId">The id of the test run; this must be ticking upwards, so
    /// a recommended value for this parameter is the current assembly version. You can use
    /// for example https://github.com/Albacore/albacore/#docs-asmver together with
    /// https://github.com/Albacore/albacore/#versionizer and
    /// https://github.com/haf/semver to manage your versions in a CI-environment.</param>
    /// <param name="tests">The performance tests to run against the subject and the alternatives.</param>
    let testPerfHistory name subject (testRunId : string) =
        testPerfHistoryWithConfig (PerfHistoryConf.Defaults name) name subject testRunId
