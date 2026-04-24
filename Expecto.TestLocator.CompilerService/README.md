# Expecto.TestLocator.CompilerService

Uses [FSharp.Compiler.Service](https://www.nuget.org/packages/FSharp.Compiler.Service) to locate Expecto tests in an F# project.

IMPORTANT: Only works for F# projects. It will not locate expecto tests in C#, VB, or any language but F#.

While it only works for F#, crawling the compiler's syntax trees allows for much higher quality test location than was previously available by reflection.