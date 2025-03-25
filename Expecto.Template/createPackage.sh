#!/usr/bin/env bash
rm -r nuget
mkdir nuget
cd nuget
mkdir content
cp ../*.fsproj content/
cp ../*.fs content/
cp -R ../.template.config content/
cp ../Expecto.Template.nuspec .
../../nuget.exe pack Expecto.Template.nuspec