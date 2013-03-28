namespace Fuchu

open System

module MbUnitTestTypes =
    open MbUnit.Framework
    
    type ATestFixture() =
        [<Test>]
        member x.ATest() = ()

        [<Test>]
        member x.AnotherTest() = failwith ""

        [<Test>]
        [<ExpectedException(typeof<ArgumentException>)>]
        member x.ATestWithExpectedException() : unit = 
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