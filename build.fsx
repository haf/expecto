#I @"packages/FAKE/tools/"
#r @"FakeLib.dll"

open Fake

let buildDir = "./build"
let nugetDir = "./.nuget"
let packagesDir = "./packages"

Target "Clean" (fun _ -> CleanDirs [buildDir])

Target "RestorePackages" RestorePackages

Target "BuildSolution" (fun _ ->        
    MSBuildWithDefaults "Build" ["./Fuchu.sln"]
    |> Log "AppBuild-Output: "
)

"Clean"
    ==> "RestorePackages"
    ==> "BuildSolution"

RunTargetOrDefault "BuildSolution"