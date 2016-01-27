namespace System
open System.Reflection

[<assembly: AssemblyKeyFileAttribute("../Fuchu.snk")>]
[<assembly: AssemblyVersionAttribute("1.0.0.0")>]
[<assembly: AssemblyFileVersionAttribute("1.0.3.0")>]
[<assembly: AssemblyTitleAttribute("Fuchu.MbUnit")>]
[<assembly: AssemblyProductAttribute("Fuchu.MbUnit")>]
[<assembly: AssemblyDescriptionAttribute("Converts Fuchu tests to MbUnit tests")>]
[<assembly: AssemblyCopyrightAttribute("Copyright Mauricio Scheffer 2016")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0.0.0"
