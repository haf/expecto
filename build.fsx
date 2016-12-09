#I @"packages/build/FAKE/tools/"
#r @"FakeLib.dll"
System.IO.Directory.SetCurrentDirectory __SOURCE_DIRECTORY__
open System
open Fake



let run cmd args dir =
  if execProcess( fun info ->
    info.FileName <- cmd
    if not( String.IsNullOrWhiteSpace dir) then
      info.WorkingDirectory <- dir
    info.Arguments <- args
  ) System.TimeSpan.MaxValue = false then
      failwithf "Error while running '%s' with args: %s" cmd args

// --------------------------------------------------------------------------------------
// Rename Logary Facades for Expecto
//  (why this is necessary - https://github.com/logary/logary/#using-logary-in-a-library)

Target "ExpectoChangeo" (fun _ ->
    ReplaceInFiles [
        "namespace Logary.Facade", "namespace Expecto.Logging"
        "namespace Logary.Facade.Messages", "namespace Expecto.Logging.Messages"
        "open Logary.Facade", "open Expecto.Logging"
        "open Logary.Facade.Messages", "open Expecto.Logging.Messages"
    ][
        "paket-files/logary/logary/src/Logary.Facade/Facade.fs"
    ]
)


// --------------------------------------------------------------------------------------
// Clean Build Detritus

Target "Clean" (fun _ ->
  !! "/**/bin/"
  ++ "/**/obj/"
  |> CleanDirs
  |> ignore
)


// --------------------------------------------------------------------------------------
// Build library, test project, & sample

let solutionFile = "Expecto"

Target "Build" (fun _ ->
  !! (solutionFile + ".sln")
  |> MSBuildRelease "" "Rebuild"
  |> ignore
)


// --------------------------------------------------------------------------------------
// Build netcore expecto library

let netcoreDir = "Expecto.netcore"
let dotnet args dir = run "dotnet" args dir

Target "DotnetBuild" (fun _ ->
  dotnet "--info"  ""
  dotnet "restore" netcoreDir
  dotnet "build"   netcoreDir
)


// --------------------------------------------------------------------------------------
// Target Dependencies

Target "All" DoNothing

"ExpectoChangeo"
  ==> "Clean"
  ==> "Build"
  <=> "DotnetBuild"

"Build"       ==> "All"
"DotnetBuild" ==> "All"

RunTargetOrDefault "All"