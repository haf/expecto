module Expecto.Differ
open System
open DiffPlex.DiffBuilder
open DiffPlex.DiffBuilder.Model

let colorizedDiff (colorizeText: ConsoleColor -> string -> string) first second =
  let first, second =
    match box first, box second with
    | (:? string as f), (:? string as s) ->
      string f, string s
    | f, s ->
      sprintf "%A" f, sprintf "%A" s

  let differ = SideBySideDiffBuilder(DiffPlex.Differ())
  let diff = differ.BuildDiffModel(first, second)

  let coloredText typ text =
    match typ with
    | ChangeType.Inserted -> colorizeText ConsoleColor.Green text
    | ChangeType.Deleted -> colorizeText ConsoleColor.Red text
    | ChangeType.Modified -> colorizeText ConsoleColor.Blue text
    | ChangeType.Imaginary -> colorizeText ConsoleColor.Yellow text
    | ChangeType.Unchanged | ChangeType.Imaginary | _ -> text

  let colorizedDiff (lines: DiffPiece seq) =
    lines
    |> Seq.map (fun line ->
      let styledLineNumber =
        match line.Position |> Option.ofNullable with
        | Some num ->
          (string num).PadLeft(2)
          |> coloredText line.Type
        | None -> "~~~"
      if line.SubPieces.Count = 0 then
        styledLineNumber + "  " + coloredText line.Type line.Text
      else
        let coloredPieces = line.SubPieces |> Seq.map (fun piece -> coloredText piece.Type piece.Text)
        coloredPieces |> fun x -> styledLineNumber + "  " + String.Join("", x)
      )
    |> fun x -> String.Join("\n", x)

  sprintf "\n---------- Actual: --------------------\n%s\n---------- Expected: ------------------\n%s\n" (colorizedDiff diff.OldText.Lines) (colorizedDiff diff.NewText.Lines)

let equals actual expected message =
//  printfn "Called with actual %A %A -- %s" actual expected message
  printfn " ___________________________ I WAS CLALLED __________________________"
  //Expect.equalDiffer (fun e a -> "hello world") actual expected message
  Expect.equalDiffer (fun expected actual -> colorizedDiff (fun color s -> s) expected actual) actual expected message