module Expecto.Diff
open System
open DiffPlex.DiffBuilder
open DiffPlex.DiffBuilder.Model
open Expecto.Logging

let private sideBySideDiffer = SideBySideDiffBuilder(DiffPlex.Differ())

let colourisedDiff actual expected =
  let actual, expected =
    match box actual, box expected with
    | (:? string as f), (:? string as s) ->
      string f, string s
    | f, s ->
      sprintf "%A" f, sprintf "%A" s

  let colouredText typ text =
    match typ with
    | ChangeType.Inserted -> ColourText.colouriseText ConsoleColor.Green text
    | ChangeType.Deleted -> ColourText.colouriseText ConsoleColor.Red text
    | ChangeType.Modified -> ColourText.colouriseText ConsoleColor.Blue text
    | ChangeType.Imaginary -> ColourText.colouriseText ConsoleColor.Yellow text
    | ChangeType.Unchanged | ChangeType.Imaginary | _ -> text

  let colouriseLine (line: DiffPiece) =
    if line.SubPieces.Count = 0 then
      colouredText line.Type line.Text
    else
      let colouredPieces = line.SubPieces |> Seq.map (fun piece -> colouredText piece.Type piece.Text)
      String.Join("", colouredPieces)

  let colourisedDiff (lines: DiffPiece seq) =
    String.Join("\n", lines |> Seq.map colouriseLine)

  let diff = sideBySideDiffer.BuildDiffModel(actual, expected)
  sprintf
    "\n---------- Actual: --------------------\n%s\n---------- Expected: ------------------\n%s\n"
    (colourisedDiff diff.OldText.Lines)
    (colourisedDiff diff.NewText.Lines)

let equals actual expected message =
  Expect.equalWithDiffPrinter colourisedDiff actual expected message
