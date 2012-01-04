open Fuchu

let test1() = assertEqual 4 (2+2)
let testcase1 = TestLabel("test1", TestCase test1)
run testcase1

