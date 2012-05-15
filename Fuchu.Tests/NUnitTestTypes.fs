namespace Fuchu

module NUnitTestTypes =

    open NUnit.Framework

    [<TestFixture>]
    type ATestFixture() =
        [<Test>]
        member x.ATest() = ()

        [<Test>]
        member x.AnotherTest() = Assert.Fail()

        [<Test>]
        [<Ignore>]
        member x.IgnoredTest() = ()

    [<TestFixture>]
    type ATestFixtureWithSetup() =
        let mutable value = 0

        static let mutable tearDownCalled = false

        static member TearDownCalled = tearDownCalled

        [<Test>]
        member x.ATest() = 
            if value <> 2 then Assert.Fail()

        [<SetUp>]
        member x.ASetup() = 
            value <- 2

        [<TearDown>]
        member x.ATeardown() = 
            tearDownCalled <- true

    [<TestFixture>]
    type ATestFixtureWithExceptionAndTeardown() =
        static let mutable tearDownCalled = false

        static member TearDownCalled = tearDownCalled

        [<Test>]
        member x.ATest() = failwith ""        

        [<TearDown>]
        member x.ATeardown() = 
            tearDownCalled <- true
