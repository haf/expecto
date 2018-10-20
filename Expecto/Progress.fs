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
  let private hideCursor = "\u001b[?25l"
  let private showCursor = "\u001b[?25h"
  let private animation = @"|/-\"

  let private color = "\u001b[1;30m"
  let private colorReset = "\u001b[0m"

  let private backStart = "\u001b[1000D"
  
  let mutable private textValue = String.Empty
  let private progressValue = None |> ref
  let private isRunning = ref false
  let mutable private isPaused = false
  let private isEnabled = not Console.IsOutputRedirected

  let text s =
    textValue <- s

  let mutable private currentLength = 0

  let update progress =
    progressValue :=
      match progress with
      | Percent p -> max p 0 |> min 100 |> Percent |> Some
      | f -> Some f

  let start() =
    lock isRunning (fun () ->
      if !isRunning then false
      else
        isRunning := true
        if isEnabled then
          Thread(fun () ->
            let start = DateTime.UtcNow
            while !isRunning do
              if not isPaused then
                lock isRunning (fun () ->
                  if !isRunning then
                    let t = (DateTime.UtcNow - start).TotalSeconds
                    let a = animation.[int t % animation.Length]
                    let progress =
                      match !progressValue with
                      | Some(Percent p) ->
                        if p=100 then [|'1';'0';'0';'%';' ';a|]
                        elif p<10 then [|' ';' ';char(48+p);'%';' ';a|]
                        else [|' ';char(48+p/10);char(48+p%10);'%';' ';a|]
                        |> String
                      | Some(Fraction (n,d)) ->
                        let ns, ds = string n, string d
                        String(' ',ds.Length-ns.Length) + ns + "/" + ds
                        + " " + string a
                      | None ->
                        String.Empty
                    currentLength <- textValue.Length + progress.Length
                    color + textValue + progress
                    + backStart + colorReset + hideCursor
                    |> originalStdout.Write
                    originalStdout.Flush()
                )
              Thread.Sleep 1000
          ).Start()
        true
    )

  let clear() =
    String(' ', currentLength) + backStart + showCursor
    |> originalStdout.Write
    originalStdout.Flush()

  let stop() =
    lock isRunning (fun() ->
      if !isRunning then
        isRunning := false
        if isEnabled then clear()
    )

  Console.CancelKeyPress |> Event.add (fun _ -> stop())

  let pause f =
    lock isRunning (fun () ->
      if !isRunning then
        isPaused <- true
        clear()
        f()
        isPaused <- false
      else f()
    )

module internal ANSIOutputWriter =
  let private colorForWhite =
    if Console.BackgroundColor = ConsoleColor.White then "\u001b[30m"
    else "\u001b[1;37m"
  let private colorReset = "\u001b[0m"
  let private colorANSI = function
    | ConsoleColor.Black -> "\u001b[30m"
    | ConsoleColor.DarkBlue -> "\u001b[34m"
    | ConsoleColor.DarkGreen -> "\u001b[32m"
    | ConsoleColor.DarkCyan -> "\u001b[36m"
    | ConsoleColor.DarkRed -> "\u001b[31m"
    | ConsoleColor.DarkMagenta -> "\u001b[35m"
    | ConsoleColor.DarkYellow -> "\u001b[33m"
    | ConsoleColor.Gray -> colorForWhite // make this white instead of "\u001b[37m"
    | ConsoleColor.DarkGray -> "\u001b[1;30m"
    | ConsoleColor.Blue -> "\u001b[1;34m"
    | ConsoleColor.Green -> "\u001b[1;32m"
    | ConsoleColor.Cyan -> "\u001b[1;36m"
    | ConsoleColor.Red -> "\u001b[1;31m"
    | ConsoleColor.Magenta -> "\u001b[1;35m"
    | ConsoleColor.Yellow -> "\u001b[1;33m"
    | ConsoleColor.White -> colorForWhite
    | _ -> ""

  let private foregroundColor = Console.ForegroundColor

  let private buffer = StringBuilder()
  let Flush() =
      lock buffer <| fun _ ->
        ProgressIndicator.pause <| fun _ ->
          buffer.ToString() |> ProgressIndicator.originalStdout.Write
          buffer.Clear() |> ignore

  let mutable private incompleteTextOutput : (string * ConsoleColor) list = []

  let private textToOutput (autoflush:bool) (fromStdOut:bool) (parts: (string * ConsoleColor) list) =
    lock buffer <| fun _ ->
      let hasEndLine =
        Seq.map fst parts
        |> Seq.where (String.IsNullOrEmpty >> not)
        |> Seq.tryLast
        |> Option.bind Seq.tryLast
        |> fun oc -> oc = Some '\n'
      if fromStdOut && not hasEndLine then
        incompleteTextOutput <- incompleteTextOutput @ parts
      else
        let parts =
          if List.isEmpty incompleteTextOutput then parts
          else
            let parts = incompleteTextOutput @ parts
            incompleteTextOutput <- []
            parts
        let mutable currentColour = foregroundColor
        parts |> List.iter (fun (text, colour) ->
          if currentColour <> colour then
            colorANSI colour |> buffer.Append |> ignore
            currentColour <- colour
          buffer.Append text |> ignore
        )
        buffer.Append colorReset |> ignore
        if autoflush then Flush()

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
    let std s = textToOutput true true [s, foregroundColor]
    new FuncTextWriter(encoding, std)
    |> Console.SetOut
    let errorEncoding = ProgressIndicator.originalStderr.Encoding
    let errorToOutput s = textToOutput true true [s, ConsoleColor.Red]
    new FuncTextWriter(errorEncoding, errorToOutput)
    |> Console.SetError
  let TextToOutput autoflush (sem:obj) (parts: (string * ConsoleColor) list) =
    lock sem <| fun _ ->
      textToOutput autoflush false parts