namespace System
open System.Reflection

[<assembly: AssemblyVersionAttribute("1.0.0.0")>]
[<assembly: AssemblyFileVersionAttribute("1.0.2.0")>]
[<assembly: AssemblyTitleAttribute("Fuchu.PerfUtil")>]
[<assembly: AssemblyProductAttribute("Fuchu.PerfUtil")>]
[<assembly: AssemblyDescriptionAttribute("Integrates Fuchu with PerfUtil")>]
[<assembly: AssemblyCopyrightAttribute("Copyright Henrik Feldt 2015")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0.0.0"
