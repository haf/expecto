namespace System
open System.Reflection

[<assembly: AssemblyVersionAttribute("1.0.0.0")>]
[<assembly: AssemblyFileVersionAttribute("1.0.1.0")>]
[<assembly: AssemblyTitleAttribute("Fuchu.FsCheck")>]
[<assembly: AssemblyProductAttribute("Fuchu.FsCheck")>]
[<assembly: AssemblyDescriptionAttribute("Integrates Fuchu with FsCheck")>]
[<assembly: AssemblyCopyrightAttribute("Copyright Mauricio Scheffer 2015")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0.0.0"
