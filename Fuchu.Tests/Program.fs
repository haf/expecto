open Fuchu
open NUnit.Framework


[<EntryPoint>]
let main args =
    let l = TestList [TestCase ignore] |> flatten |> List.length
    if l <> 1
        then failwith "wrong list length in flatten"
    let test1() = Assert.AreEqual(4, 2+2)
    let testcase1 = TestLabel("test1", TestCase test1)
    run testcase1