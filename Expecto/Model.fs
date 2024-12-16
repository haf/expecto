namespace Expecto

open System
open System.Threading
open System.Threading.Tasks

type SourceLocation =
  { sourcePath: string; lineNumber: int }
  static member empty = { sourcePath = ""; lineNumber = 0 }

type FsCheckConfig =
  { /// The maximum number of tests that are run.
    maxTest: int
    /// The size to use for the first test.
    startSize: int
    /// The size to use for the last test, when all the tests are passing. The size increases linearly between Start- and EndSize.
    endSize: int
    /// If set, the seed to use to start testing. Allows reproduction of previous runs.
    replay: (uint64 * uint64) option
    /// The Arbitrary instances on this class will be merged in back to front order, i.e. instances for the same generated type at the front
    /// of the list will override those at the back. The instances on Arb.Default are always known, and are at the back (so they can always be
    /// overridden)
    arbitrary: Type list
    /// Callback when the test case had input parameters generated.
    receivedArgs: FsCheckConfig
               -> (* test name *) string
               -> (* test number *) int
               -> (* generated arguments *) obj list
               -> Async<unit>
    /// Callback when the test case was successfully shrunk
    successfulShrink: FsCheckConfig
                   -> (* test name *) string
                   -> (* shrunk new arguments *) obj list
                   -> Async<unit>
    /// Callback when the test case has finished
    finishedTest: FsCheckConfig
               -> (* test name *) string
               -> Async<unit>
    /// If set, suppresses the output from the test if the test is successful.
    quietOnSuccess: bool
    /// The maximum number of tests where values are rejected, e.g. as the result of ==>
    maxRejected: int
  }

  static member defaultConfig =
    { maxTest = 200
      startSize = 1
      endSize = 100
      replay = None
      arbitrary = []
      receivedArgs = fun _ _ _ _ -> async.Return ()
      successfulShrink = fun _ _ _ -> async.Return ()
      finishedTest = fun _ _ -> async.Return ()
      quietOnSuccess = true
      maxRejected = 1000
    }

/// Actual test function; either an async one, or a synchronous one.
type TestCode =
  | Sync of stest: (unit -> unit)
  | SyncWithCancel of stest: (CancellationToken -> unit)
  | Async of atest: Async<unit>
  | AsyncFsCheck of testConfig: FsCheckConfig option *
                    stressConfig: FsCheckConfig option *
                    test: (FsCheckConfig -> Async<unit>)

/// The parent state (watching the tests as a tree structure) will influence
/// the child tests state. By following rules, if parent test state is:
///     - Focused will elevate all Normal child tests to Focused.
///              Focused and Pending child tests will not change state(behavior)
///     - Normal will not influence the child tests states(behavior).
///     - Pending will elevate all Normal and Focused child tests to Pending.
///              Pending child test will not change state(behavior)
type FocusState =
  /// The default state of a test that will be evaluated
  | Normal
  /// The state of a test that will be ignored by Expecto
  | Pending
  /// The state of a test that will be evaluated
  /// All other test marked with Normal or Pending will be ignored
  | Focused

type SequenceMethod =
  | Synchronous
  | SynchronousGroup of string
  | InParallel

/// Test tree – this is how you compose your tests as values. Since
/// any of these can act as a test, you can pass any of these DU cases
/// into a function that takes a Test.
type Test =
  /// A test case is a function from unit to unit, that can be executed
  /// by Expecto to run the test code.
  | TestCase of code:TestCode * state:FocusState
  /// A collection/list of tests.
  | TestList of tests:Test list * state:FocusState
  /// A labelling of a Test (list or test code).
  | TestLabel of label:string * test:Test * state:FocusState
  /// Require sequenced for a Test (list or test code).
  | Sequenced of SequenceMethod * Test

type ExpectoException(msg) = inherit Exception(msg)
type PassWithMessage(msg) = inherit ExpectoException(msg)
type AssertException(msg) = inherit ExpectoException(msg)
type FailedException(msg) = inherit ExpectoException(msg)
type IgnoreException(msg) = inherit ExpectoException(msg)
/// Represents an error during test discovery.
type TestDiscoveryException(msg) = inherit ExpectoException(msg)
/// Represents a null test value during test discovery.
type NullTestDiscoveryException(msg) = inherit TestDiscoveryException(msg)

/// Marks a top-level test for scanning
/// The test will run even if PTest is also present.
[<AttributeUsage(AttributeTargets.Method ||| AttributeTargets.Property ||| AttributeTargets.Field)>]
type TestsAttribute() = inherit Attribute()

/// Allows to mark a test as Pending (will be skipped/ignored if no other TestAttribute is present)
/// Is a fast way to exclude some tests from running.
/// If FTest or Test is also present then this attribute will be ignored.
[<AttributeUsage(AttributeTargets.Method ||| AttributeTargets.Property ||| AttributeTargets.Field)>]
type PTestsAttribute() = inherit Attribute()

/// Allows to mark a test as FocusState.Focused (will be run and will change the behavior for
/// all other tests marked as FocusState.Normal to be ignored)
/// Is a fast way to exclude some tests from running.
/// The test will run even if PTest is also present. Have priority over TestAttribute.
[<AttributeUsage(AttributeTargets.Method ||| AttributeTargets.Property ||| AttributeTargets.Field)>]
type FTestsAttribute() = inherit Attribute()

type private TestNameHolder() =
  [<ThreadStatic;DefaultValue>]
  static val mutable private name : string
  static member Name
      with get () = TestNameHolder.name
      and  set name = TestNameHolder.name <- name

