# Fuchu #

**Fuchu** is a test library for .NET, supporting C# and VB.NET but with a special focus on F#.   
It draws heavily from Haskell's [test-framework](http://batterseapower.github.com/test-framework/) and [HUnit](http://hunit.sourceforge.net/).   
You can read about the rationale and underlying concepts in [this blog post](http://bugsquash.blogspot.com/2012/06/fuchu-functional-test-library-for-net.html).

## Binaries ##

Binaries are available on [NuGet](http://nuget.org/packages?q=Fuchu).

## Writing tests ##

Here's the simplest test possible:


    open Fuchu

    let simpleTest = 
        testCase "A simple test" <| 
            fun _ -> Assert.Equal("2+2", 4, 2+2)

Tests can be grouped (with arbitrary nesting):

    let tests = 
        testList "A test group" [
            testCase "one test" <|
                fun _ -> Assert.Equal("2+2", 4, 2+2)
            testCase "another test" <|
                fun _ -> Assert.Equal("3+3", 3, 3+3)
        ]

In C#:

    static Test ATest {
        get {
            return Test.List("A test group", new[] {
                Test.Case("one test", () => Assert.Equal("2+2", 4, 2+2)),
                Test.Case("another test", () => Assert.Equal("3+3", 3, 3+3)),
            });
        }
    }
    
The first parameter in the assertions describes the assertion. This is usually an optional parameter in most test frameworks; in Fuchu it's required to foster descriptive failures, so you'll get a failure like "3+3 Expected value 3, actual 6" instead of just "Expected value 3, actual 6".

For more examples, including a few ways to do common things in other test frameworks like setup/teardown and parameterized tests, see the [F# tests](https://github.com/mausch/Fuchu/blob/master/Fuchu.Tests/Tests.fs) and the [C# tests](https://github.com/mausch/Fuchu/blob/master/Fuchu.CSharpTests/Program.cs)

## Assertions ##

Fuchu is mainly oriented to test organization. Although it does have a few basic assertions, you're encouraged to write your own specialized assertions for each project (they're only a couple of lines in F#), or use some other library for assertions, like [Unquote](http://code.google.com/p/unquote/), [FsUnit](https://github.com/dmohl/FsUnit), or even MbUnit or NUnit.

## Running tests ##

The test runner is the test assembly itself. It's recommended to compile your test assembly as a console application. You can run a test directly like this:

    run simpleTest // or runParallel
    
which returns 1 if any tests failed, otherwise 0. Useful for returning to the operating system as error code. Or you can mark the top-level test in each test file with the `[<Tests>]` attribute, then define your main like this:

    [<EntryPoint>]
    let main args = defaultMainThisAssembly args
    
This `defaultMainThisAssembly` function admits a "/m" parameter passed through the command-line to run tests in parallel.
    
You can single out tests by filtering them by name. For example:

    tests
    |> Test.filter (fun s -> s.EndsWith "another test")
    |> run

You can use the F# REPL to run tests this way.

In C#:

    static int Main(string[] args) {
        return ATest.Run(); // or RunParallel()
    }

Or scanning for tests marked with the [Tests] attribute:

    static int Main(string[] args) {
        return Tests.DefaultMainThisAssembly(args);
    }

## FsCheck integration ##

Reference [FsCheck](http://fscheck.codeplex.com/) and Fuchu.FsCheck to test properties:


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
    
In C# (can't override FsCheck config at the moment):

    static Test Properties =
        Test.List("FsCheck", new[] {
            FsCheck.Property("Addition is commutative",
                                (int a, int b) => a + b == b + a),
            FsCheck.Property("Product is distributive over addition",
                                (int a, int b, int c) => a * (b + c) == a * b + a * c),
        });

You can freely mix FsCheck properties with regular test cases and test lists.

## More examples ##

Some projects using Fuchu:

* [FsSql](https://github.com/mausch/FsSql/tree/master/FsSql.Tests)
* [FsFormlets](https://github.com/mausch/FsFormlets/tree/master/Formlets.Tests)
* UrchiNet ([C#](https://github.com/mausch/UrchiNet/blob/master/UrchiNet.CSharpTests/Tests.cs) and [F#](https://github.com/mausch/UrchiNet/blob/master/UrchiNet.Tests/Tests.fs))
* [Figment](https://github.com/mausch/Figment/tree/master/Figment.Tests)
* [NHWebConsole](https://github.com/mausch/NHWebConsole/tree/master/NHWebConsole.Tests)
* [Fleece](https://github.com/mausch/Fleece/blob/master/Tests/Tests.fs)
* [EdmundsNet](https://github.com/mausch/EdmundsNet/blob/master/Tests/Tests.fs)
* [Suave](https://github.com/SuaveIO/suave/blob/master/Tests/Program.fs)
