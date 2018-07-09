namespace Expecto

open System
open System.Threading
open System.IO
open System.Text
open System.Runtime.InteropServices

#nowarn "9"

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
  let originalStderr = stderr
  let private hideCursor = "\x1B[?25l"
  let private showCursor = "\x1B[?25h"
  let private animation = @"|/-\"

  let private color = "\x1b[1;33m"
  let private colorReset = "\x1B[0m"
  
  let mutable private textValue = String.Empty
  let private progressValue = Percent 0 |> ref
  let private isRunning = ref false
  let private isEnabled = not Console.IsOutputRedirected

  let text s =
    textValue <- s

  let mutable private currentLength = 0

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
                      String(' ',ds.Length-ns.Length) + ns + "/" + ds
                      + " " + string a
                  currentLength <- textValue.Length + progress.Length
                  color + textValue + progress
                  + String('\b', currentLength) + colorReset + hideCursor
                  |> originalStdout.Write
                  originalStdout.Flush()
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
          String(' ', currentLength) + String('\b', currentLength) + showCursor
          |> originalStdout.Write
          originalStdout.Flush()
    )

  let pause f =
    lock isRunning (fun () ->
      if !isRunning then
        stop(); f(); start() |> ignore
      else f()
    )

module internal ANSIOutputWriter =
  let private colorForWhite =
    if Console.BackgroundColor = ConsoleColor.White then "\x1B[30m"
    else "\x1B[1;37m"
  let private colorReset = "\x1B[0m"
  let private colorANSI = function
    | ConsoleColor.Black -> "\x1b[30m"
    | ConsoleColor.DarkBlue -> "\x1b[34m"
    | ConsoleColor.DarkGreen -> "\x1b[32m"
    | ConsoleColor.DarkCyan -> "\x1b[36m"
    | ConsoleColor.DarkRed -> "\x1b[31m"
    | ConsoleColor.DarkMagenta -> "\x1b[35m"
    | ConsoleColor.DarkYellow -> "\x1b[33m"
    | ConsoleColor.Gray -> "\x1b[37m"
    | ConsoleColor.DarkGray -> "\x1b[1;30m"
    | ConsoleColor.Blue -> "\x1b[1;34m"
    | ConsoleColor.Green -> "\x1b[1;32m"
    | ConsoleColor.Cyan -> "\x1b[1;36m"
    | ConsoleColor.Red -> "\x1b[1;31m"
    | ConsoleColor.Magenta -> "\x1b[1;35m"
    | ConsoleColor.Yellow -> "\x1b[1;33m"
    | ConsoleColor.White -> colorForWhite
    | _ -> ""

  let private foregroundColor = Console.ForegroundColor

  let private buffer = StringBuilder()
  let private textToOutput (parts: (string * ConsoleColor) list) =
    lock buffer <| fun _ ->
      let mutable currentColour = foregroundColor
      parts |> List.iter (fun (text, colour) ->
        if currentColour <> colour then
          colorANSI colour |> buffer.Append |> ignore
          currentColour <- colour
        buffer.Append text |> ignore
      )
      buffer.Append colorReset |> ignore

  let Flush() =
      lock buffer <| fun _ ->
        ProgressIndicator.pause <| fun _ ->
          buffer.ToString() |> ProgressIndicator.originalStdout.Write
          buffer.Clear() |> ignore
  let Close() =
    Flush()
    Console.SetOut ProgressIndicator.originalStdout
    Console.SetError ProgressIndicator.originalStderr

  module WindowsConsole =
    open Microsoft.FSharp.NativeInterop
    [<DllImport("Kernel32")>]
    extern void* private GetStdHandle(int _nStdHandle)
    [<DllImport("Kernel32")>]
    extern bool private GetConsoleMode(void* _hConsoleHandle, int* _lpMode)
    [<DllImport("Kernel32")>]
    extern bool private SetConsoleMode(void* _hConsoleHandle, int _lpMode)
    let enableVTMode() =
      let INVALID_HANDLE_VALUE = nativeint -1
      let STD_OUTPUT_HANDLE = -11
      let ENABLE_VIRTUAL_TERMINAL_PROCESSING = 0x0004
      let handle = GetStdHandle(STD_OUTPUT_HANDLE)
      if handle <> INVALID_HANDLE_VALUE then
        let mode = NativePtr.stackalloc<int> 1
        if GetConsoleMode(handle, mode) then
          let value = NativePtr.read mode
          let value = value ||| ENABLE_VIRTUAL_TERMINAL_PROCESSING
          SetConsoleMode(handle, value) |> ignore
  do
#if NETSTANDARD2_0
    if RuntimeInformation.IsOSPlatform OSPlatform.Windows then
      WindowsConsole.enableVTMode()
#else
    if Environment.OSVersion.Platform = PlatformID.Win32NT then
      WindowsConsole.enableVTMode()
#endif
    ProgressIndicator.originalStdout.Flush()
    let encoding = ProgressIndicator.originalStdout.Encoding
    let std s = textToOutput [s, foregroundColor]
    new FuncTextWriter(encoding, std)
    |> Console.SetOut
    let errorEncoding = ProgressIndicator.originalStderr.Encoding
    let errorToOutput s =
      textToOutput [s, ConsoleColor.Red]
      Flush()
    new FuncTextWriter(errorEncoding, errorToOutput)
    |> Console.SetError
  let TextToOutput (sem:obj) (parts: (string * ConsoleColor) list) =
    lock sem <| fun _ ->
      textToOutput parts