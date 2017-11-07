dotnet msbuild %~dp0nugetclient.proj /t:Pack /p:NuspecFile=%4 /p:PackageOutputPath=%3
