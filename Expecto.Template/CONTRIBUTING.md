# Contributing

## Updating the template

Updating the template is simple. 
In short, [/content/expecto](./content/expecto/) contains the files for the end user.
You can modify these files and it will reflect in the template output.

For example, if you want to update the template framework version, you just edit the `<TargetFramework>` 
in [/content/expecto/ExpectoTemplate.fsproj](./content/expecto/ExpectoTemplate.fsproj) just like you would with any other project.


If you want to do more advanced edits, like add a new template, consult the [template authoring docs](https://learn.microsoft.com/en-us/dotnet/core/tutorials/cli-templates-create-template-package)


## Packing

From the Expecto.Template folder, run `dotnet pack`.
You'll probably want to update the PackageVersion first.


## Testing Locally

`dotnet pack` and then `dotnet new install path/to/nupkg/you-just-packed`

## Publishing

```pwsh
$nugetApiKey = 'your-api-key'
rm ./bin/Release/*.nupkg
dotnet pack -c Release -o ./bin/Release/
ls ./bin/Release/*.nupkg |% { dotnet nuget push $_ --source https://api.nuget.org/v3/index.json --api-key $nugetApiKey}
```
