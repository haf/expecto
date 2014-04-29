namespace Fuchu

open System

module MbUnitTestTypes =
    open MbUnit.Framework
    
    type ATestFixture() =
        [<Test>]
        member x.ATest() = ()

        [<Test>]
        member x.AnotherTest() : unit = 
            failwith ""

        [<Test>]
        [<ExpectedException(typeof<ArgumentException>)>]
        member x.ATestWithExpectedException() : unit = 
            invalidArg "" ""

        [<Test>]
        [<ExpectedException(typeof<ArgumentNullException>)>]
        member x.ATestWithFailedExpectedException() : unit = 
            invalidArg "" ""

        [<Test>]
        [<Ignore>]
        member x.IgnoredTest() = ()

    type ATestFixtureWithSetup() =
        let mutable value = 0

        static let mutable tearDownCalled = false

        static member TearDownCalled = tearDownCalled

        [<Test>]
        member x.ATest() = 
            if value <> 2 then Assert.Fail("")

        [<SetUp>]
        member x.ASetup() = 
            value <- 2

        [<TearDown>]
        member x.ATeardown() = 
            tearDownCalled <- true

    [<TestFixture>]
    [<Category "fixture category">]
    type ATestFixtureWithExceptionAndTeardown() =
        static let mutable tearDownCalled = false

        static member TearDownCalled = tearDownCalled

        [<Test>]
        [<Category "test category">]
        member x.ATest() = failwith ""

        [<TearDown>]
        member x.ATeardown() = 
            tearDownCalled <- true

    [<TestFixture>]
    type ATestFixtureWithRow() =
        [<Test>]
        [<Row(1000, 10, 100)>]
        [<Row(4195835, 3145729, 1.3338196)>]
        member x.DivTest(numerator: double, denominator: double, result: double) =
            Assert.AreApproximatelyEqual(result, numerator / denominator, 0.00001)

    type ATestFixtureWithStaticTestFactories() =
        [<StaticTestFactory>]
        static member Tests() = 
            let suite = TestSuite("suite name")
            Seq.iter suite.Children.Add
                [
                    TestCase("test 1", fun () -> ())
                    TestCase("test 2", fun () -> ())
                ]
            [suite]

    type ATestFixtureWithStaticTestFactories2() =
        [<StaticTestFactory>]
        static member Tests() =
            [
                TestCase("test 1", fun () -> ())
                TestCase("test 2", fun () -> ())
            ]

    [<AbstractClass>]
    type AbstractWithTest() =
        [<Test>]
        member x.ATest() = ()