namespace System
open System.Reflection

[<assembly: AssemblyVersionAttribute("0.4.0.0")>]
[<assembly: AssemblyFileVersionAttribute("0.4.0.0")>]
[<assembly: AssemblyTitleAttribute("Fuchu.PerfUtil")>]
[<assembly: AssemblyProductAttribute("Fuchu.PerfUtil")>]
[<assembly: AssemblyDescriptionAttribute("Integrates Fuchu with PerfUtil")>]
[<assembly: AssemblyCopyrightAttribute("Copyright Henrik Feldt 2014")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.4.0.0"
