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

"Clean"
  ==> "Build"
  <=> "DotnetBuild"

"Build"       ==> "All"
"DotnetBuild" ==> "All"

RunTargetOrDefault "All"