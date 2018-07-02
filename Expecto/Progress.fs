namespace Expecto

open System
open System.Threading
open System.IO
open System.Text

type private FuncTextWriter(encoding:Encoding, write:string->unit) =
  inherit TextWriter()
  override __.Encoding = encoding
  override __.Write (s:string) = write s
  override __.WriteLine (s:string) = s + "\n" |> write
  override __.WriteLine() = write "\n"

type Progress =
  | Percent of int
  | Fraction of int * int

module internal ProgressIndicator =
  let originalStdout = stdout
  let private hideCursor = "\x1B[?25l"
  let private showCursor = "\x1B[?25h"
  let private animation = @"|/-\"
  
  let mutable private textValue = String.Empty
  let private progressValue = Percent 0 |> ref
  let private isRunning = ref false
  let private isEnabled = not Console.IsOutputRedirected

  let text s =
    textValue <- s

  let update progress =
    progressValue :=
      match progress with
      | Percent p -> max p 0 |> min 100 |> Percent
      | f -> f

  let start() =
    lock isRunning (fun () ->
      if !isRunning then false
      else
        isRunning := true
        if isEnabled then
          hideCursor |> stdout.Write
          Thread(fun () ->
            let start = DateTime.UtcNow
            while !isRunning do
              lock isRunning (fun () ->
                if !isRunning then
                  let t = (DateTime.UtcNow - start).TotalSeconds
                  let a = animation.[int t % animation.Length]
                  let progress =
                    match !progressValue with
                    | Percent p ->
                      if p=100 then [|'1';'0';'0';'%';' ';a|]
                      elif p<10 then [|' ';' ';char(48+p);'%';' ';a|]
                      else [|' ';char(48+p/10);char(48+p%10);'%';' ';a|]
                      |> String
                    | Fraction (n,d) ->
                      let ns, ds = string n, string d
                      String(' ',ds.Length-ns.Length) + ns + "/" + ds + " " + string a
                  textValue + progress + String('\b', textValue.Length + progress.Length)
                  |> originalStdout.Write
                  Console.Out.Flush()
              )
              Thread.Sleep 1000
          ).Start()
        true
    )

  let stop() =
    lock isRunning (fun() ->
      if !isRunning then
        isRunning := false
        if isEnabled then
          String(' ', 80) + String('\b', 80) + showCursor
          |> originalStdout.Write
          Console.Out.Flush()
    )

  let pause f =
    lock isRunning (fun () ->
      if !isRunning then
        stop(); f(); start() |> ignore
      else f()
    )

type ANSIOutputWriter() =
  let buffer = StringBuilder()
  let colorForWhite =
    if Console.BackgroundColor = ConsoleColor.White then "\x1B[90m" else "\x1B[97m"
  let colorReset = "\x1B[0m"

  let colorANSI = function
    | ConsoleColor.White -> colorForWhite
    | ConsoleColor.Gray -> "\x1B[31m"
    | ConsoleColor.DarkGray -> "\x1B[90m"
    | ConsoleColor.Yellow -> "\x1B[93m"
    | ConsoleColor.Red -> "\x1B[91m"
    | ConsoleColor.Blue -> "\x1B[33m"
    | ConsoleColor.Magenta -> "\x1B[95m"
    | ConsoleColor.Cyan -> "\x1B[96m"
    | ConsoleColor.Green -> "\x1B[92m"
    | _ -> "\x1B[334m"

  let foregroundColor = Console.ForegroundColor

  let textToOutput (parts: (string * ConsoleColor) list) =
    lock buffer <| fun _ ->
      let mutable currentColour = foregroundColor
      parts |> List.iter (fun (text, colour) ->
        if currentColour <> colour then
          colorANSI colour |> buffer.Append |> ignore
          currentColour <- colour
        buffer.Append text |> ignore
      )
      buffer.Append colorReset |> ignore

  let originalStdout =
    let orig = ProgressIndicator.originalStdout
    let textToOutput s = textToOutput [s, foregroundColor]
    let tw = new FuncTextWriter(orig.Encoding, textToOutput)
    Console.SetOut tw
    orig

  member __.TextToOutput (sem:obj) (parts: (string * ConsoleColor) list) =
    lock sem <| fun _ ->
      textToOutput parts

  member __.Flush() =
      lock buffer <| fun _ ->
        ProgressIndicator.pause <| fun _ ->
          buffer.ToString() |> originalStdout.Write
          buffer.Clear() |> ignore

  interface IDisposable with
    member __.Dispose() =
      Console.SetOut originalStdout

// TODO
// stderr
// flush on error and printfn
// stdout dispose etc
// fix colours