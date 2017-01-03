# Devguide

## Setup

 1. **EditorConfig** – If you haven't already, install the EditorConfig
    extension to prevent any conflicts between your default F# formatting
    conventions and the ones used by this project.
    * Visual Studio - [EditorConfig VS Extension][ec-vs]
    * vscode - [EditorConfig VScode Extension][ec-vsc]
 1. Install the latest version of the [**netcore SDK**][netcore-sdk]
 1. You need [albacore][ac] installed to build like I do – it needs to build
    with the `Rakefile` or I won't accept the PR.
 1. New features:
   - Make your test for your change
   - Make your change
   - If appropriate, write docs in README.md.

## Code style

 - Spaces after function names.
 - Spaces after parameter names like so `param : typ`.
 - 2 space indentation.
 - Use `lowerCaseCamelCase`, also on properties.
 - Interfaces *do not* start with `I`. Can you use a function instead, perhaps?
 - LF (not CRLF) in source files.
 - Prefer flat namespaces.
 - Prefer namespaces containing *only* what a consumer needs to use – no
   utilities should be public if it can be helped.
 - Follow the existing style of the library.
 - For single-argument function calls, prefer `fn par` over `par |> fn`. For
   multiple-argument function calls, `par3 |> fn par1 par2` is OK.
 - No final newline in files, please.
 - Open specify `open` directives for the used namespaces and modules.
 - Prefer to cluster `open` directives at the top of the file.

 [ec-vs]: https://marketplace.visualstudio.com/items?itemName=EditorConfigTeam.EditorConfig
 [ec-vsc]: https://marketplace.visualstudio.com/items?itemName=EditorConfig.EditorConfig
 [netcore-sdk]: https://github.com/dotnet/cli#installers-and-binaries
 [ac]: https://github.com/albacore/albacore
