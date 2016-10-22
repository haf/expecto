namespace Expecto

module PerfTests =
    open global.PerfUtil

    type Y = { a : string; b : int }

    type Serialiser =
        inherit ITestable
        abstract member Serialise<'a> : 'a -> unit

    type MySlowSerialiser() =
        interface ITestable with
            member x.Name = "Slow Serialiser"
            member x.Init () = ()
            member x.Fini () = ()
        interface Serialiser with
            member x.Serialise _ =
                System.Threading.Thread.Sleep(30)

    type FastSerialiser() =
        interface ITestable with
            member x.Name = "Fast Serialiser"
            member x.Init () = ()
            member x.Fini () = ()
        interface Serialiser with
            member x.Serialise _ =
                System.Threading.Thread.Sleep(10)

    type FastSerialiserAlt() =
        interface ITestable with
            member x.Name = "Fast Serialiser Alt"
            member x.Init () = ()
            member x.Fini () = ()
        interface Serialiser with
            member x.Serialise _ =
                System.Threading.Thread.Sleep(20)

    let alts : Serialiser list = [ FastSerialiser(); FastSerialiserAlt() ]
    let subj = MySlowSerialiser() :> Serialiser
    let perfTest a b = perfTest a b 5

    let normal_serialisation : PerfTest<Serialiser> list = [
        perfTest "serialising string" <| fun s ->
            s.Serialise("wowowow")
        perfTest "serialising record" <| fun s ->
            s.Serialise { a = "hello world"; b = 42 }
        ]

    [<Tests>]
    let tests =
        testList "performance comparison tests" [
            testPerfImpls "implementations of Serialiser" subj alts normal_serialisation
            testPerfHistory "historical MySlowSerialiser" subj "v1.2.3" normal_serialisation
        ]

