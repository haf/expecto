@echo off

dotnet tool restore
dotnet run --project ./Build/Build.fsproj -- %*
