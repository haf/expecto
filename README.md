# expecto

[![Build Status](https://travis-ci.org/haf/expecto.svg?branch=master)](https://travis-ci.org/haf/expecto)
[![NuGet Badge](https://buildstats.info/nuget/FAKE)](https://www.nuget.org/packages/expecto)

Expecto is a unit testing library for F#. It's a fork of Fuchu aiming to be
properly updated and pushed to nuget when PRs come in. It aims to be an
opinionated testing framework with batteries included, while still being
compositional (just like Suave and Logary are).

![Sample output](docs/sample-output-2.png)

  * [expecto](#expecto)
    * [Installing](#installing)
    * [Testing "Hello world"](#testing-hello-world)
    * [Running tests](#running-tests)
      * [runTests](#runtests)
      * [runTestsInAssembly](#runtestsinassembly)
      * [testList for grouping](#testlist-for-grouping)
      * [Filtering with filter](#filtering-with-filter)
      * [Focusing tests](#focusing-tests)
      * [Pending tests](#pending-tests)
    * [Expectations](#expectations)
      * [Expect module](#expect-module)
      * [Performance module](#performance-module)
        * [Example](#example)
    * [main argv – how to run console apps](#main-argv--how-to-run-console-apps)
      * [The config](#the-config)
    * [Contributing](#contributing)
    * [FsCheck usage](#fscheck-usage)
    * [BenchmarkDotNet usage](#benchmarkdotnet-usage)
    * [You're not alone\!](#youre-not-alone)
    * [Sending e\-mail on failure – custom printers](#sending-e-mail-on-failure--custom-printers)
    * [About test parallelism](#about-test-parallelism)
    * [About upgrading from Fuchu](#about-upgrading-from-fuchu)
      * [Why the strange name?](#why-the-strange-name)
      * [What does 'expected to have type TestCode' mean?](#what-does-expected-to-have-type-testcode-mean)

## Installing

In your paket.dependencies:

```
nuget Expecto
nuget Expecto.BenchmarkDotNet
nuget Expecto.FsCheck
```

Tests should be first-class values so that you can move them around and execute
them in any context that you want.

Let's have look at what an extensive unit test suite looks like when running
with Expecto:

![Sample output from Logary](docs/sample-output-logary.png)

## Testing "Hello world"

The test runner is the test assembly itself. It's recommended to compile your
test assembly as a console application. You can run a test directly like this:

```fsharp
open Expecto

[<Tests>]
let tests =
  testCase "yes" <| fun () ->
    let subject = "Hello world"
    Expect.equal subject "Hello World"
                 "The strings should equal"

[<EntryPoint>]
let main args =
  runTestsInAssembly defaultConfig args
```

The base class is called `Expect`, containing functions you can use to assert
with. A testing library without a good assertion library is like love without
kisses.

Now compile and run! `xbuild Sample.fsproj && mono --debug bin/Debug/Sample.exe`

## Running tests

Here's the simplest test possible:

```fsharp
open Expecto

let simpleTest =
  testCase "A simple test" <| fun _ ->
    let expected = 4
    Expect.equal expected (2+2) "2+2 = 4"
```

Then run it like this, e.g. in the interactive or through a console app.

```fsharp
runTests defaultConfig simpleTest
```

which returns 1 if any tests failed, otherwise 0. Useful for returning to the
operating system as error code.

### `runTests`

Signature `ExpectoConfig -> Test -> int`. Runs the passed tests with the passed
configuration record.

### `runTestsWithArgs`

Signature `ExpectoConfig -> string[] -> Test -> int`. Runs the passed tests
and also overrides the passed `ExpectoConfig` with the command line parameters.

### `runTestsInAssembly`

Signature `ExpectoConfig -> string[] -> int`. Runs the tests in the current
assembly and also overrides the passed `ExpectoConfig` with the command line
parameters.

### `testList` for grouping

Tests can be grouped (with arbitrary nesting):

```fsharp
[<Tests>]
let tests =
  testList "A test group" [
    testCase "one test" <| fun _ ->
      Expect.equal (2+2) 4 "2+2"

    testCase "another test that fails" <| fun _ ->
      Expect.equal (3+3) 5 "3+3"

    testCaseAsync "this is an async test" async {
      do! async.Return()
    }
  ]
```

Also have a look at [the
samples](https://github.com/haf/expecto/blob/master/Expecto.Sample/Expecto.Sample.fs).

### Filtering with `filter`

You can single out tests by filtering them by name (e.g. in the
interactive/REPL). For example:

```fsharp
open Expecto
open MyLib.Tests
integrationTests // from MyLib.Tests
|> Test.filter (fun s -> s.EndsWith "another test") // the filtering function
|> runTests defaultConfig
```

### Focusing tests

It is often convenient, when developing to be able to run a subset of specs.
Expecto allows you to focus specific test cases or tests list by putting `f` before *testCase* or *testList* or `F` before attribute *Tests*(when reflection tests discovery is used).

```fsharp
open Expecto

[<FTests>]
let someFocusedTest = testCase "will run" <| fun _ -> Expect.equal (2+2) 4 "2+2"
[<Tests>]
let someUnfocusedTest = test "skipped" { Expect.equal (2+2) 1 "2+2?" }
```

or

```fsharp
open Expecto

[<Tests>]
let focusedTests =
  testList "unfocused list" [
    ftestList "focused list" [
      testCase "will run" <| fun _ -> Expect.equal (2+2) 4 "2+2"
      ftestCase "will run" <| fun _ -> Expect.equal (2+2) 4 "2+2"
      test "will run" { Expect.equal (2+2) 4 "2+2" }
    ]
    testList "unfocused list" [
      testCase "skipped" <| fun _ -> Expect.equal (2+2) 1 "2+2?"
      ftestCase "will run" <| fun _ -> Expect.equal (2+2) 4 "2+2"
      test "skipped" { Expect.equal (2+2) 1 "2+2?" }
      ftest "will run" { Expect.equal (2+2) 4 "2+2" }
    ]
    testCase "skipped" <| fun _ -> Expect.equal (2+2) 1 "2+2?"
  ]
```

Expecto accepts the command line argument `--fail-on-focused-tests`, which checks if focused tests exist.
This parameter can be set in build scripts and allows CI servers to reject commits that accidentally included focused tests.

### Pending tests

You can mark an individual spec or container as Pending. This will prevent the
spec (or specs within the list) from running.  You do this by adding a `p`
before *testCase* or *testList* or `P` before *Tests* attribute (when reflection
tests discovery is used).

```fsharp
open Expecto

[<PTests>]
let skippedTestFromReflectionDiscovery = testCase "skipped" <| fun _ ->
    Expect.equal (2+2) 4 "2+2"

[<Tests>]
let myTests =
  testList "normal" [
    testList "unfocused list" [
      ptestCase "skipped" <| fun _ -> Expect.equal (2+2) 1 "2+2?"
      testCase "will run" <| fun _ -> Expect.equal (2+2) 4 "2+2"
      ptest "skipped" { Expect.equal (2+2) 1 "2+2?" }
    ]
    testCase "will run" <| fun _ -> Expect.equal (2+2) 4 "2+2"
    ptestCase "skipped" <| fun _ -> Expect.equal (2+2) 1 "2+2?"
    ptestList "skipped list" [
      testCase "skipped" <| fun _ -> Expect.equal (2+2) 1 "2+2?"
      ftestCase "skipped" <| fun _ -> Expect.equal (2+2) 1 "2+2?"
    ]
  ]
```

### Sequenced tests

You can mark an individual spec or container as Sequenced.
This will make sure these tests are run sequentially.
This can be useful for timeout and performance testing.

```fsharp
[<Tests>]
let timeout =
    testSequenced <| testList "Timeout" [
      testCase "fail" <| fun _ ->
        let test = TestCase(Test.timeout 10 (fun _ -> Thread.Sleep 100), Normal)
        let result = evalSilent test |> sumTestResults
        result.failed.Length ==? 1
      testCase "pass" <| fun _ ->
        let test = TestCase(Test.timeout 1000 ignore, Normal)
        let result = evalSilent test |> sumTestResults
        result.passed.Length ==? 1
    ]
```

## Expectations

All expect-functions have the signature `actual -> expected -> string -> unit`,
leaving out `expected` when obvious from the function.

### `Expect` module

This module is your main entry-point when asserting.

 - `throws`
 - `throwsC`
 - `throwsT`
 - `isNone`
 - `isSome`
 - `isChoice1Of2`
 - `isChoice2Of2`
 - `isNull`
 - `isNotNull`
 - `isLessThan`
 - `isLessThanOrEqual`
 - `isGreaterThan`
 - `isGreaterThanOrEqual`
 - `notEqual`
 - `isFalse`
 - `isTrue`
 - `sequenceEqual`
 - `floatClose : Accuracy -> float -> float -> string -> unit` - Expect the
   floats to be within the combined absolute and relative accuracy given by
   `abs(a-b) <= absolute + relative * max (abs a) (abs b)`. Default accuracy
   available are: `accLow = {absolute=1e-6; relative=1e-3}`,
   `accMedium = {absolute=1e-8; relative=1e-5}`,
   `accHigh = {absolute=1e-10; relative=1e-7}`,
   `accVeryHigh = {absolute=1e-12; relative=1e-9}`.
 - `sequenceStarts` - Expect the sequence `subject` to start with `prefix`. If
   it does not then fail with `format` as an error message together with a
   description of `subject` and `prefix`.
 - `isAscending` - Expect the sequence `subject` to be ascending. If it does not
   then fail with `format` as an error message.
 - `isDescending` - Expect the sequence `subject` to be descending. If it does
   not then fail with `format` as an error message.
 - `stringContains` – Expect the string `subject` to contain `substring` as part
   of itself.  If it does not, then fail with `format` and `subject` and
   `substring` as part of the error message.
 - `stringStarts` – Expect the string `subject` to start with `prefix` and if it
   does not then fail with `format` as an error message together with a
   description of `subject` and `prefix`.
 - `stringEnds` - Expect the string `subject` to end with `suffix`. If it does
   not then fail with `format` as an error message together with a description
   of `subject` and `suffix`.
 - `stringHasLength` - Expect the string `subject` to have length equals
   `length`. If it does not then fail with `format` as an error message together
   with a description of `subject` and `length`.
 - `contains : 'a seq -> 'a -> string -> unit` – Expect the sequence to contain
   the item.
 - `containsAll: 'a seq -> 'a seq -> string -> unit` - Expect the sequence
   contains all elements from second sequence (not taking into account an order
   of elements)
 - `distribution: 'a seq -> Map<'a, uint32> -> string -> unit` - Expect the sequence contains all elements from map (first element in tuple is an item expected to be in sequence, second is a positive number of its occurrences in a sequence). Function is not taking into account an order of elements.
 - `streamsEqual` – Expect the streams to be byte-wise identical.
 - `isFasterThan : (unit -> 'a) -> (unit -> 'a) -> string -> unit` – Expect the
    first function to be faster than the second function with the passed string
    message, printed on failure. See the next section on Performance for example
    usage.
 - `isFasterThanSub` – Like the above but with passed function signature of
   `Performance.Measurer<unit,'a> -> 'a`, allowing you to do setup and teardown
   of your subject under test (the function) before calling the Measurer. See
   the next section on Performance for example usage.

### `Performance` module

Expecto supports testing that an implementation is faster than another. Use it
by calling `Expect.isFasterThan` wrapping your `Test` in `testSequenced`.

![Sample output](./docs/half-is-faster.png)

This function makes use of a statistical test called [Welch's t-test](https://en.wikipedia.org/wiki/Welch's_t-test).
It starts with the null hypothesis that the functions mean execution times are the same.
The functions are run alternately increasing the sample size to test this hypothesis.

Once a confidence level of 99.99% is reached that this hypothesis is incorrect it stops and reports the results.
If the performance is very close the test will declare them equal when there is 99.99% confidence they differ by less than 0.5%.
99.99% is chosen such that if a test set has 100 performance tests a false test failure would be reported once in many more than 100 runs.

This results in a performance test that is very quick to run (the greater the difference the quicker it will run).
Also, because it is a relative test it can normally be run across all configurations as part of unit testing.

The functions must return the same result for same input. Note that since
Expecto also has a FsCheck integration, your outer (sequenced) test could be
the property test, generating random data, and your TestCode/function body/
actual test could be an assertion that for the same (random instance) of test-
data, one function should be faster than the other.

From `Expect.isFasterThanSub`, these results are possible (all of which generate
a test failure, except the MetricLessThan case):

```fsharp
  type 'a CompareResult =
    | ResultNotTheSame of result1:'a * result2:'a
    | MetricTooShort of sMax:SampleStatistics * machineResolution:SampleStatistics
    | MetricLessThan of s1:SampleStatistics * s2:SampleStatistics
    | MetricMoreThan of s1:SampleStatistics * s2:SampleStatistics
    | MetricEqual of s1:SampleStatistics * s2:SampleStatistics
```

You can explore these cases yourself with `Expecto.Performance.timeCompare`,
should you wish to.

#### Example

All of the below tests pass.

```fsharp
[<Tests>]
let performance =
  testSequenced <| testList "performance" [

    testCase "1 <> 2" <| fun _ ->
      let test () =
        Expect.isFasterThan (fun () -> 1) (fun () -> 2) "1 equals 2 should fail"
      assertTestFailsWithMsgContaining "same" (test, Normal)

    testCase "half is faster" <| fun _ ->
      Expect.isFasterThan (fun () -> repeat10000 log 76.0)
                          (fun () -> repeat10000 log 76.0 |> ignore; repeat10000 log 76.0)
                          "half is faster"

    testCase "double is faster should fail" <| fun _ ->
      let test () =
        Expect.isFasterThan (fun () -> repeat10000 log 76.0 |> ignore; repeat10000 log 76.0)
                            (fun () -> repeat10000 log 76.0)
                            "double is faster should fail"
      assertTestFailsWithMsgContaining "slower" (test, Normal)

    ptestCase "same function is faster should fail" <| fun _ ->
      let test () =
        Expect.isFasterThan (fun () -> repeat100000 log 76.0)
                            (fun () -> repeat100000 log 76.0)
                            "same function is faster should fail"
      assertTestFailsWithMsgContaining "equal" (test, Normal)

    testCase "matrix" <| fun _ ->
      let n = 100
      let rand = Random 123
      let a = Array2D.init n n (fun _ _ -> rand.NextDouble())
      let b = Array2D.init n n (fun _ _ -> rand.NextDouble())
      let c = Array2D.zeroCreate n n

      let reset() =
        for i = 0 to n-1 do
            for j = 0 to n-1 do
              c.[i,j] <- 0.0

      let mulIJK() =
        for i = 0 to n-1 do
          for j = 0 to n-1 do
            for k = 0 to n-1 do
              c.[i,k] <- c.[i,k] + a.[i,j] * b.[j,k]

      let mulIKJ() =
        for i = 0 to n-1 do
          for k = 0 to n-1 do
            let mutable t = 0.0
            for j = 0 to n-1 do
              t <- t + a.[i,j] * b.[j,k]
            c.[i,k] <- t
      Expect.isFasterThanSub (fun measurer -> reset(); measurer mulIKJ ())
                             (fun measurer -> reset(); measurer mulIJK ())
                             "ikj faster than ijk"

    testCase "popcount" <| fun _ ->
      let test () =
        Expect.isFasterThan (fun () -> repeat10000 (popCount16 >> int) 987us)
                            (fun () -> repeat10000 (popCount >> int) 987us)
                            "popcount 16 faster than 32 fails"
      assertTestFailsWithMsgContaining "slower" (test, Normal)
  ]
```

A failure would look like this:

```
[13:23:19 ERR] performance/double is faster failed in 00:00:00.0981990.
double is faster. Expected f1 (0.3067 ± 0.0123 ms) to be faster than f2 (0.1513 ± 0.0019 ms) but is ~103% slower.
```

## `main argv` – how to run console apps

Parameters available if you use `Tests.runTestsInAssembly defaultConfig argv` in your code:

 - `--debug`: Extra verbose output for your tests.
 - `--sequenced`: Run all tests in sequence.
 - `--parallel`: (default) Run all tests in parallel.
 - `--filter <hiera>`: Filter a specific hierarchy to run.
 - `--filter-test-list <substring>`: Filter a specific test list to run.
 - `--filter-test-case <substring>`: Filter a specific test case to run.
 - `--run [<tests1> <test2> ...]`: Run only provided tests.
 - `--list-tests`: Doesn't run tests, print out list of tests instead.
 - `--summary`: Prints out summary after all tests are finished

### The config

If you prefer using F# to configure the tests, you can set the properties of the
ExpetoConfig record, that looks like:

```fsharp
{ /// Whether to run the tests in parallel. Defaults to true, because your
  /// code should not mutate global state by default.
  parallel : bool
  /// An optional filter function. Useful if you only would like to run a
  /// subset of all the tests defined in your assembly.
  filter   : Test -> Test
  /// Allows the test printer to be parametised to your liking.
  printer : TestPrinters }
```

By doing a `let config = { defaultConfig with parallel = true }`, for example.

## Contributing

Please see the [Devguide](./DEVGUIDE.md).

## FsCheck usage

Reference [FsCheck](https://github.com/fscheck/FsCheck) and Expecto.FsCheck to
test properties:

```fsharp
let config = { FsCheck.Config.Default with MaxTest = 10000 }

let properties =
  testList "FsCheck" [
    testProperty "Addition is commutative" <| fun a b ->
      a + b = b + a

    // you can also override the FsCheck config
    testPropertyWithConfig config "Product is distributive over addition" <|
      fun a b c ->
        a * (b + c) = a * b + a * c
  ]

run properties
```

You can freely mix testProperty with testCase and testList.

## BenchmarkDotNet usage

The integration with
[BenchmarkDotNet](https://perfdotnet.github.io/BenchmarkDotNet/index.htm).

```fsharp
open Expecto
open BenchmarkDotNet

module Types =
  type Y = { a : string; b : int }

type Serialiser =
  abstract member Serialise<'a> : 'a -> unit

type MySlowSerialiser() =
  interface Serialiser with
    member x.Serialise _ =
      System.Threading.Thread.Sleep(30)

type FastSerialiser() =
  interface Serialiser with
    member x.Serialise _ =
      System.Threading.Thread.Sleep(10)

type FastSerialiserAlt() =
  interface Serialiser with
    member x.Serialise _ =
     System.Threading.Thread.Sleep(20)

type Serialisers() =
  let fast, fastAlt, slow =
    FastSerialiser() :> Serialiser,
    FastSerialiserAlt() :> Serialiser,
    MySlowSerialiser() :> Serialiser

  [<Benchmark>]
  member x.FastSerialiserAlt() = fastAlt.Serialise "Hello world"

  [<Benchmark>]
  member x.SlowSerialiser() = slow.Serialise "Hello world"

  [<Benchmark(Baseline = true)>]
  member x.FastSerialiser() = fast.Serialise "Hello world"

open Types

[<Tests>]
let tests =
  testList "performance tests" [
    benchmark<Serialisers> "three serialisers" benchmarkConfig ignore
  ]
```

In the current code-base I'm just printing the output to the console; and by
default all tests are run in parallel; so you'll need to use `--sequenced` as
input to your exe, or set parallel=false in the config to get valid results.

To read more about how to benchmark with BenchmarkDotNet, see its [Getting
started](http://benchmarkdotnet.org/GettingStarted.htm) guide.

Happy benchmarking!

## You're not alone!

Others have discovered the beauty of tests-as-values in easy-to-read F#.

* [Suave](https://github.com/SuaveIO/suave/tree/master/src/Suave.Tests)
* [Logary](https://github.com/logary/logary)
* [Unquote has built-in support](https://github.com/SwensenSoftware/unquote/pull/128)
* [Visual Studio Plugin for
  Expecto](https://www.nuget.org/packages/Expecto.VisualStudio.TestAdapter/) –
  just add the `nuget Expecto.VisualStudio.TestAdapter version_in_path: true` to
  your paket file and you're off to the races!

![Expecto VS Test Plugin](./docs/expecto-vs-addon.jpeg "Easy to get started even for Enterprise Developers").

## Sending e-mail on failure – custom printers

The printing mechanism in Expecto is based on the [Logary
Facade](https://github.com/logary/logary#the-logary-facade-adapter), which
grants some privileges, like being able to use **any** Logary target to print.
Just follow the above link to learn how to initialise Logary. Then if you wanted
to get notified over e-mail whenever one of your tests fail, configure Logary
with `Logary.Targets.Mailgun`:

```fsharp
open Logary
open Logary.Configuration
open Logary.Adapters.Facade
open Logary.Targets
open Hopac
open Mailgun
open System.Net.Mail

let main argv =
  let mgc =
    MailgunLogaryConf.Create(
      MailAddress("travis@example.com"),
      [ MailAddress("Your.Mail.Here@example.com") ],
      { apiKey = "deadbeef-2345678" },
      "example.com", // sending domain of yours
      Error) // cut-off level

  use logary =
    withLogaryManager "MyTests" (
      withTargets [
        LiterateConsole.create LiterateConsole.empty "stdout"
        Mailgun.create mgc "mail"
      ]
      >> withRules [
        Rule.createForTarget "stdout"
        Rule.createForTarget "mail"
      ])
    |> run

  // initialise Logary Facade with Logary proper:
  LogaryFacadeAdapter.initialise<Expecto.Logging.Logger> logary

  // run all tests
  Tests.runTestsInAssembly defaultConfig args
```

## About test parallelism

Since the default is to run all of your tests in parallel, it's important that
you don't use global variables, global singletons or mutating code. If you do,
you'll have to slow down all of your tests by sequencing them (or use locks in
your testing code).

Furthermore, `printfn` and sibling functions aren't thread-safe, i.e. a given
string may be logged in many passes and concurrent calls to printfn and
Console.X-functions have their outputs interleaved. If you want to log from
tests, you can use code like:

```fsharp
open Expecto.Logging
open Expecto.Logging.Message

let logger = Log.create "MyTests"

// stuff here

testCase "reading prop" <| fun _ ->
  let subject = MyComponent()
  // this will output to the right test context:
  logger.info(
    eventX "Has prop {property}"
    >> setField "property" subject.property)
  Expect.equal subject.property "Goodbye" "Should have goodbye as its property"
```

## About upgrading from Fuchu

In VsCode, search files for: `Assert.Equal\s*\(\s*((;|.)+?)\s*,\s*(.*?),\s*(.*?)\)`
and replace with `Expect.equal $4 $3 $1`.

### Why the strange name?

![Expecto expecto](./docs/expecto-patronus-2000x1126.png "This is actually because nuget won't let me publish them with the name 'Expecto', plain and simple.")

### What does 'expected to have type TestCode' mean?

If you get an error message like this:

```
This expression was expected to have type    'TestCode'    but here has type    'unit'
```

It means that you have code like `testCase "abc" <| Expect.equal ...`. Instead
you should create a function like so: `testCase "abc" <| fun _ -> Expect.equal
...`.
