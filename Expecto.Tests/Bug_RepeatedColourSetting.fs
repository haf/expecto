module Bug_RepeatedColourSetting

open Expecto
open Expecto.Logging


[<Tests>]
let tests =
  ftest "Colour can be set repeatedly" {
    let colours = [|Colour0; Colour256|]
    colours |> Array.iter ANSIOutputWriter.setColourLevel
    
    Expect.equal (ANSIOutputWriter.getColour ()) (Array.last colours) "Colour should be the last set value"
  }
