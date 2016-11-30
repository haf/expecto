#I @"packages/build/FAKE/tools/"
#r @"FakeLib.dll"

open Fake

let solutionFile = "Expecto"

// --------------------------------------------------------------------------------------
// Build library, test project, & sample

Target "Build" (fun _ ->
    // We would like to build only one solution
    !! (solutionFile + ".sln")
    |> MSBuildRelease "" "Rebuild"
    |> ignore
)

RunTargetOrDefault "Build"