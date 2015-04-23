namespace System
open System.Reflection

[<assembly: AssemblyKeyFileAttribute("../Fuchu.snk")>]
[<assembly: AssemblyVersionAttribute("0.5.0.0")>]
[<assembly: AssemblyFileVersionAttribute("0.5.0.0")>]
[<assembly: AssemblyTitleAttribute("Fuchu.MbUnit")>]
[<assembly: AssemblyProductAttribute("Fuchu.MbUnit")>]
[<assembly: AssemblyDescriptionAttribute("Converts Fuchu tests to MbUnit tests")>]
[<assembly: AssemblyCopyrightAttribute("Copyright Mauricio Scheffer 2015")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.5.0.0"
