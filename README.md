# expecto

[![Build Status](https://travis-ci.org/haf/expecto.svg?branch=master)](https://travis-ci.org/haf/expecto)

Expecto is a unit testing library for F#. It's a fork of Fuchu aiming to be
properly updated and pushed to nuget when PRs come in. It aims to be an
opinionated testing framework with batteries included, while still being
compositional (just like Suave and Logary are). Former README follows:

In your paket.dependencies:

```
nuget Expecto
```

Tests should be first-class values so that you can move them around and execute
them in any context that you want.

## Writing tests

Here's the simplest test possible:

```fsharp
open Expecto

let simpleTest =
  testCase "A simple test" <| fun _ ->
    Expect.equal  ("2+2", 4, 2+2)
```

Tests can be grouped (with arbitrary nesting):

```fsharp
let tests =
  testList "A test group" [
    testCase "one test" <|
      fun _ -> Assert.Equal("2+2", 4, 2+2)
    testCase "another test" <|
      fun _ -> Assert.Equal("3+3", 3, 3+3)
  ]
```

The first parameter in the assertions describes the assertion. This is usually an optional parameter in most test frameworks; in Expecto it's required to foster descriptive failures, so you'll get a failure like "3+3 Expected value 3, actual 6" instead of just "Expected value 3, actual 6".

For more examples, including a few ways to do common things in other test frameworks like setup/teardown and parameterized tests, see the [F# tests](https://github.com/haf/expecto/blob/master/Expecto.Tests/Tests.fs).

## Assertions

The base class is called `Expect`, containing F# functions you can use to assert
with. A testing library without a good assertion library is like love without
kisses.

## Running tests

The test runner is the test assembly itself. It's recommended to compile your test assembly as a console application. You can run a test directly like this:

```fsharp
runParallel simpleTest // or runParallel
```

which returns 1 if any tests failed, otherwise 0. Useful for returning to the operating system as error code. Or you can mark the top-level test in each test file with the `[<Tests>]` attribute, then define your main like this:

```fsharp
open Expecto

[<EntryPoint>]
let main args =
  defaultMainThisAssembly args
```

This `defaultMainThisAssembly` function admits a "/m" parameter passed through the command-line to run tests in parallel.

You can single out tests by filtering them by name. For example:

```fsharp
tests
|> Test.filter (fun s -> s.EndsWith "another test")
|> run
```

You can use the F# REPL to run tests this way.

## FsCheck integration

Reference [FsCheck](https://github.com/fscheck/FsCheck) and Expecto.FsCheck to
test properties:

```fsharp
let config = { FsCheck.Config.Default with MaxTest = 10000 }

let properties =
  testList "FsCheck" [
    testProperty "Addition is commutative" <|
      fun a b ->
        a + b = b + a

    // you can also override the FsCheck config
    testPropertyWithConfig config "Product is distributive over addition" <|
      fun a b c ->
        a * (b + c) = a * b + a * c
  ]

run properties
```

You can freely mix FsCheck properties with regular test cases and test lists.

## PerfUtil integration ##

The integration with Eirik's PerfUtil project.

```fsharp
open global.PerfUtil

module Types =
    type Y = { a : string; b : int }

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
        testPerfImpls "implementations of Serialiser" subj alts normal_serlialisation
        testPerfHistory "historical MySlowSerialiser" subj "v1.2.3" normal_serlialisation
    ]
```

This example shows both a comparison performance test between MySlowSerialiser, FastSerialiser and
FastSerialiserAlt: `testPerfImpls` and a historical comparison of MySlowSerialiser alone
which saves an xml file next to the dll on every run.

You can find detailed docs in the source code of PerfUtil.fs on all parameters and data
structures. All things that can be configured with PerfUtil can be configured with the
`conf` parameter to `testPerfImplsWithConfig` and `testPerfHistoryWithConfig`.

The functions are discoverable by starting with `testPerf*`.

Handle the results explicitly by giving a config with a value of `handleResults`. Use
that if you want to export the data to e.g. CSV or TSV.

## You're not alone!

Others have discovered the beauty of tests-as-values in easy-to-read F#.

* [Suave](https://github.com/SuaveIO/suave/tree/master/src/Suave.Tests)
* [Logary](https://github.com/logary/logary)
