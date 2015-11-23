namespace System
open System.Reflection

[<assembly: AssemblyKeyFileAttribute("../Fuchu.snk")>]
[<assembly: AssemblyVersionAttribute("1.0.0.0")>]
[<assembly: AssemblyFileVersionAttribute("1.0.2.0")>]
[<assembly: AssemblyTitleAttribute("Fuchu")>]
[<assembly: AssemblyProductAttribute("Fuchu")>]
[<assembly: AssemblyDescriptionAttribute("Functional test library")>]
[<assembly: AssemblyCopyrightAttribute("Copyright Mauricio Scheffer 2015")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0.0.0"
