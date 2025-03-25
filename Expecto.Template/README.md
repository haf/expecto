[![NuGet Pre Release](https://buildstats.info/nuget/Expecto.Template?includePreReleases=true)](https://www.nuget.org/packages/Expecto.Template)
[![Build Status](https://travis-ci.org/MNie/Expecto.Template.svg?branch=master)](https://travis-ci.org/MNie/Expecto.Template)

# Expecto .NET Template
It's a template for [Expecto](https://github.com/haf/expecto) F# test library.

# How to use it?
* install template as a .NET template from nuget
* install template as a .NET template locally

# How to install it as a template from NuGet?
* run `dotnet new -i Expecto.Template`
* create a new project executing `dotnet new expecto -n PROJECT_NAME -o FOLDER_NAME -lang F#`

# How to install it as a template locally?
* download repository
* run command `dotnet new -i PATH`, where PATH is a path to a folder where project is located
* create new project executing `dotnet new expecto -n PROJECT_NAME -o FOLDER_NAME -lang F#` (since there is a [known issue in dotnet cli](https://github.com/dotnet/templating/issues/1401) `-lang F#` parameter is currently required)
* `dotnet restore`
* `dotnet run` or `dotnet watch run` or `dotnet test` or `dotnet watch test`

# How it works?
* ![dotnet run && dotnet test](https://raw.githubusercontent.com/MNie/Expecto.Template/master/images/expecto.gif)
* ![dotnet watch run](https://raw.githubusercontent.com/MNie/Expecto.Template/master/images/expectowatch.gif)
