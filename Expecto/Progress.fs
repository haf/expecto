namespace Expecto

open System
open System.Threading
open System.IO
open System.Text
open System.Runtime.InteropServices
open Expecto.Logging

#nowarn "9"

type Progress =
  | Percent of int
  | Fraction of int * int

module internal ProgressIndicator =
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

  let start () =
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
                    let t = (DateTime.UtcNow - start).TotalMilliseconds / 100.
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

                    let value = color + textValue + progress + backStart + colorReset + hideCursor
                    ANSIOutputWriter.writeAndFlushRaw value
                )
              Thread.Sleep 60
          ).Start()
        true
    )

  let clear () =
    let value = String(' ', currentLength) + backStart + showCursor
    ANSIOutputWriter.writeAndFlushRaw value

  let stop () =
    lock isRunning <| fun () ->
      if !isRunning then
        isRunning := false
        if isEnabled then clear ()

  Console.CancelKeyPress |> Event.add (fun _ -> stop ())

  ANSIOutputWriter.FlushStart |> Event.add (fun () ->
    Monitor.Enter isRunning
    if !isRunning then
      isPaused <- true
      clear ()
      // logically, now the caller will execute the flush
  )

  ANSIOutputWriter.FlushEnd |> Event.add (fun () ->
    // logically, now the caller has executed the flush
    if !isRunning then
      isPaused <- false
    Monitor.Exit isRunning
  )

  (* The above corresponds to:

      let pause f =
        lock isRunning <| fun () ->
          if !isRunning then
            isPaused <- true
            clear ()
            f ()
            isPaused <- false
          else
            f ()

  *)
