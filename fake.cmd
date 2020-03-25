@echo off

dotnet tool restore
dotnet tool run fake %*
