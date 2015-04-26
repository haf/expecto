namespace System
open System.Reflection

[<assembly: AssemblyVersionAttribute("0.6.0.0")>]
[<assembly: AssemblyFileVersionAttribute("0.6.0.0")>]
[<assembly: AssemblyTitleAttribute("Fuchu.PerfUtil")>]
[<assembly: AssemblyProductAttribute("Fuchu.PerfUtil")>]
[<assembly: AssemblyDescriptionAttribute("Integrates Fuchu with PerfUtil")>]
[<assembly: AssemblyCopyrightAttribute("Copyright Henrik Feldt 2015")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.6.0.0"
