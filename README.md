# Fuchu #

**Fuchu** is a test library for .NET, supporting C# and VB.NET but with a special focus on F#.   
It draws heavily from Haskell's [test-framework](http://batterseapower.github.com/test-framework/) and [HUnit](http://hunit.sourceforge.net/).   
You can read about the rationale and underlying concepts in [this blog post](http://bugsquash.blogspot.com/2012/06/fuchu-functional-test-library-for-net.html).

## Writing tests ##

Firstly, it should be noted that Fuchu does not implement any assertions at the moment. You're free to use NUnit, MbUnit, or any other test framework for assertions.

Here's the simplest test possible:


    open Fuchu
    open NUnit.Framework

    let simpleTest = 
        testCase "A simple test" <| 
            fun _ -> Assert.AreEqual(4, 2+2)

Tests can be grouped:

    let tests = 
        testList "A test group" [
            testCase "one test" <|
                fun _ -> Assert.AreEqual(4, 2+2)
            testCase "another test" <|
                fun _ -> 
        ]

You can also use a more compact syntax if you don't mind the operators:

    let simpleTest = 
        "A simple test" =>
            fun _ -> Assert.AreEqual(4, 2+2)

    let tests = 
        "A test group" =>> [
            "one test" =>
                fun _ -> Assert.AreEqual(4, 2+2)
            "another test" =>
                fun _ -> 
        ]

## Running tests ##

The test runner is the test assembly itself. It's recommended to compile your test assembly as a console application. You can run a test directly like this:

    run simpleTest // or runParallel
    
which returns 1 if any tests failed, otherwise 0. Useful for returning to the operating system as error code. Or you can mark the top-level test in each test file with the `[<Tests>]` attribute, then define your main like this:

    [<EntryPoint>]
    let main args = defaultMainThisAssembly args
    
You can single out tests by filtering them by name. For example:

    tests
    |> Test.filter (fun s -> s.EndsWith "another test")
    |> run

You can use the F# REPL to run tests this way.

