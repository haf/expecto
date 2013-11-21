#I @"packages/FAKE/tools/"
#r @"FakeLib.dll"

open Fake
open Fake.ProcessHelper

let buildDir = "./build"
let nugetDir = "./.nuget"
let packagesDir = "./packages"

Target "Clean" (fun _ -> CleanDirs [buildDir])

Target "RestorePackages" RestorePackages

Target "BuildSolution" (fun _ ->
    MSBuildWithDefaults "Build" ["./Fuchu.sln"]
    |> Log "AppBuild-Output: "
)

Target "Test" <| fun _ ->
    let errorCode = 
        [
            @"Fuchu.Tests\bin\Debug\Fuchu.Tests.exe"
            @"Fuchu.CSharpTests\bin\Debug\Fuchu.CSharpTests.exe"
        ]
        |> Seq.map (fun p -> asyncShellExec { defaultParams with Program = p })
        |> Async.Parallel
        |> Async.RunSynchronously
        |> Array.sum
    if errorCode <> 0 then failwith "Error in tests"

"BuildSolution" <== ["Clean"; "RestorePackages"]
"Test" <== ["BuildSolution"]

RunTargetOrDefault "BuildSolution"