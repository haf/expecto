namespace Expecto

open System
open System.Threading

module internal ProgressIndicator =

  let private hideCursor = "\x1B[?25l"
  let private showCursor = "\x1B[?25h"
  let private animation = @"|/-\"
  
  let mutable private textValue = String.Empty

  let private percentValue = ref 0
  let private isRunning = ref false

  let text s = textValue <- s

  let update percent =
    percentValue := max percent 0 |> min 100

  let start() =
    lock isRunning (fun () ->
      if !isRunning then false
      else
        isRunning := true
        if not Console.IsOutputRedirected then
          hideCursor + textValue |> stdout.Write
          Thread(fun () ->
              while true do
                lock isRunning (fun () ->
                  if !isRunning then
                    let p = !percentValue
                    let a = animation.[p % animation.Length]
                    let percent =
                      if p=100 then [|'1';'0';'0';'%';' ';a|]
                      elif p<10 then [|' ';' ';char(48+p);'%';' ';a|]
                      else [|' ';char(48+p/10);char(48+p%10);'%';' ';a|]
                    String percent + "\b\b\b\b\b\b" |> stdout.Write
                )
                Thread.Sleep 250
          ).Start()
        true
    )

  let stop() =
    lock isRunning (fun() ->
      if !isRunning then
        isRunning := false
        if not Console.IsOutputRedirected then
          let lineLength = textValue.Length + 7
          let erase = String('\b', lineLength)
          erase + String(' ', lineLength) + erase + showCursor |> stdout.Write
    )

  let pause f =
    lock isRunning (fun () ->
      if !isRunning then
        stop(); f(); start() |> ignore
      else f()
    )