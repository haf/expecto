#I @"packages/FAKE/tools/"
#r @"FakeLib.dll"

open Fake
open Fake.ProcessHelper

let buildDir = "./build"

Target "Clean" (fun _ -> CleanDirs [buildDir])

Target "BuildSolution" (fun _ ->
    MSBuildWithDefaults "Build" ["./Fuchu.sln"]
    |> Log "AppBuild-Output: "
)

Target "Test" <| fun _ ->
    let errorCode = 
        [
            "Fuchu.Tests"
            "Fuchu.CSharpTests"
        ]
        |> Seq.map (fun t -> t @@ "bin" @@ "Debug" @@ (t + ".exe"))
        |> Seq.map (fun p -> if not isMono then p,null else "mono",p)
        |> Seq.map (fun (p,a) -> asyncShellExec { defaultParams with Program = p; CommandLine = a })
        |> Async.Parallel
        |> Async.RunSynchronously
        |> Array.sum
    if errorCode <> 0 then failwith "Error in tests"

// "BuildSolution" <== ["Clean" ]
"Test" <== ["BuildSolution"]

RunTargetOrDefault "BuildSolution"
