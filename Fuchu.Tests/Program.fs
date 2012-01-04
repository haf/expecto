open Fuchu

let test1 = TestCase (fun() -> assertEqual 4 (2+2)) |> withLabel "test1"
run test1

