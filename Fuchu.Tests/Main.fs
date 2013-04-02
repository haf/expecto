module Main

open Fuchu

[<MbUnit.Framework.StaticTestFactory>]
let gallioEntryPoint() =
    Fuchu.MbUnit.fromAssembly (System.Reflection.Assembly.GetExecutingAssembly())

[<EntryPoint>]
let main args = 
    defaultMainThisAssembly args
