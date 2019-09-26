module Expecto.Differ
open System
open DiffPlex.DiffBuilder
open DiffPlex.DiffBuilder.Model

let colourisedDiff (colouriseText: ConsoleColor -> string -> string) first second =
  let first, second =
    match box first, box second with
    | (:? string as f), (:? string as s) ->
      string f, string s
    | f, s ->
      sprintf "%A" f, sprintf "%A" s

  let differ = SideBySideDiffBuilder(DiffPlex.Differ())
  let diff = differ.BuildDiffModel(first, second)

  let colouredText typ text =
    match typ with
    | ChangeType.Inserted -> colouriseText ConsoleColor.Green text
    | ChangeType.Deleted -> colouriseText ConsoleColor.Red text
    | ChangeType.Modified -> colouriseText ConsoleColor.Blue text
    | ChangeType.Imaginary -> colouriseText ConsoleColor.Yellow text
    | ChangeType.Unchanged | ChangeType.Imaginary | _ -> text

  let colourisedDiff (lines: DiffPiece seq) =
    lines
    |> Seq.map (fun line ->
      let styledLineNumber =
        match line.Position |> Option.ofNullable with
        | Some num ->
          (string num).PadLeft(2)
          |> colouredText line.Type
        | None -> "~~~"
      if line.SubPieces.Count = 0 then
        styledLineNumber + "  " + colouredText line.Type line.Text
      else
        let colouredPieces = line.SubPieces |> Seq.map (fun piece -> colouredText piece.Type piece.Text)
        colouredPieces |> fun x -> styledLineNumber + "  " + String.Join("", x)
      )
    |> fun x -> String.Join("\n", x)

  sprintf "\n---------- Actual: --------------------\n%s\n---------- Expected: ------------------\n%s\n" (colourisedDiff diff.OldText.Lines) (colourisedDiff diff.NewText.Lines)

let equals actual expected message =
  Expect.equalDiffer (fun colouriser expected actual -> colourisedDiff (colouriser) expected actual) actual expected message