namespace Expecto

open System

[<ReferenceEquality>]
type FlatTest =
  { name      : string
    test      : TestCode
    state     : FocusState
    focusOn   : bool
    sequenced : SequenceMethod }
  member x.shouldSkipEvaluation =
    match x.focusOn, x.state with
    | _, Pending -> Some "The test or one of its parents is marked as Pending"
    | true, Normal -> Some "The test is skipped because other tests are Focused"
    | _ -> None

[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module Test =
  /// Compute the child test state based on parent test state
  let computeChildFocusState parentState childState =
    match parentState, childState with
    | Focused, Pending -> Pending
    | Pending, _ -> Pending
    | Focused, _ -> Focused
    | Normal, _ -> childState

  /// Is focused set on at least one test
  let rec isFocused test =
    match test with
    | TestLabel (_,_,Focused)
    | TestCase (_,Focused)
    | TestList (_,Focused) -> true
    | TestLabel (_,_,Pending)
    | TestList (_,Pending)
    | TestCase _ -> false
    | TestLabel (_,test,Normal)
    | Sequenced (_,test) -> isFocused test
    | TestList (tests,Normal) -> List.exists isFocused tests

  /// Flattens a tree of tests
  let toTestCodeList test =
    let isFocused = isFocused test
    let rec loop parentName testList parentState sequenced =
      function
      | TestLabel (name, test, state) ->
        let fullName =
          if String.IsNullOrEmpty parentName
            then name
            else parentName + "/" + name
        loop fullName testList (computeChildFocusState parentState state) sequenced test
      | TestCase (test, state) ->
        { name=parentName
          test=test
          state=computeChildFocusState parentState state
          focusOn = isFocused
          sequenced=sequenced } :: testList
      | TestList (tests, state) -> List.collect (loop parentName testList (computeChildFocusState parentState state) sequenced) tests
      | Sequenced (sequenced,test) -> loop parentName testList parentState sequenced test
    loop null [] Normal InParallel test

  let fromFlatTests (tests:FlatTest list) =
    TestList(
      List.map (fun t ->
        TestLabel(t.name, Sequenced(t.sequenced, TestCase (t.test, t.state)), t.state)
      ) tests
    , Normal)

  let shuffle (test:Test) =
    let tests =
      toTestCodeList test
      |> List.toArray
    Array.shuffleInPlace tests
    Array.toList tests
    |> fromFlatTests

  /// Change the FocusState by applying the old state to a new state
  /// Note: this is not state replacement!!!
  ///
  /// Used in replaceTestCode and the order is intended for scenario:
  ///  1. User wants to automate some tests and his intent is not to change
  ///      the test state (use Normal), so this way the current state will be preserved
  ///
  /// Don't see the use case: the user wants to automate some tests and wishes
  /// to change the test states
  let rec translateFocusState newFocusState =
    function
    | TestCase (test, oldFocusState) -> TestCase(test, computeChildFocusState oldFocusState newFocusState)
    | TestList (testList, oldFocusState) -> TestList(testList, computeChildFocusState oldFocusState newFocusState)
    | TestLabel (label, test, oldFocusState) -> TestLabel(label, test, computeChildFocusState oldFocusState newFocusState)
    | Sequenced (sequenced,test) -> Sequenced (sequenced,translateFocusState newFocusState test)

  /// Recursively replaces TestCodes in a Test.
  /// Check translateFocusState for focus state behaviour description.
  let rec replaceTestCode (f:string -> TestCode -> Test) =
    function
    | TestLabel (label, TestCase (test, childState), parentState) ->
      f label test
      |> translateFocusState (computeChildFocusState parentState childState)
    | TestCase (test, state) ->
      f null test
      |> translateFocusState state
    | TestList (testList, state) -> TestList (List.map (replaceTestCode f) testList, state)
    | TestLabel (label, test, state) -> TestLabel (label, replaceTestCode f test, state)
    | Sequenced (sequenced,test) -> Sequenced (sequenced,replaceTestCode f test)

  /// Filter tests by name.
  let filter pred =
    toTestCodeList
    >> List.filter (fun t -> pred t.name)
    >> List.map (fun t ->
        let test = TestLabel (t.name, TestCase (t.test, t.state), t.state)
        match t.sequenced with
        | InParallel ->
          test
        | s ->
          Sequenced (s,test)
      )
    >> (fun x -> TestList (x,Normal))

  /// Applies a timeout to a test.
  let timeout timeout (test: TestCode) : TestCode =
    let timeoutAsync testAsync =
      async {
        try
          let! async = Async.StartChild(testAsync, timeout)
          do! async
        with :? TimeoutException ->
          let ts = TimeSpan.FromMilliseconds (float timeout)
          raise <| AssertException(sprintf "Timeout (%A)" ts)
      }

    match test with
    | Sync test -> async { test() } |> timeoutAsync |> Async
    | SyncWithCancel test ->
      SyncWithCancel (fun ct ->
        Async.StartImmediate(async { test ct } |> timeoutAsync)
      )
    | Async test -> timeoutAsync test |> Async
    | AsyncFsCheck (testConfig, stressConfig, test) ->
      AsyncFsCheck (testConfig, stressConfig, test >> timeoutAsync)

