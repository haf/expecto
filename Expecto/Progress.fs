namespace Expecto

open System
open System.Threading

module internal ProgressIndicator =

  let private hideCursor = "\x1B[?25l"
  let private showCursor = "\x1B[?25h"
  let private animation = @"|/-\"
  
  let mutable private textValue = String.Empty
  let mutable private eraseLine = String.Empty
  let private percentValue = ref 0
  let private isRunning = ref false

  let text s =
    textValue <- s
    let lineLength = textValue.Length + 7
    eraseLine <- String('\b', lineLength)

  let update percent =
    percentValue := max percent 0 |> min 100

  let start() =
    lock isRunning (fun () ->
      if !isRunning then false
      else
        isRunning := true
        if not Console.IsOutputRedirected then
          hideCursor |> stdout.Write
          Thread(fun () ->
              let start = DateTime.UtcNow
              while !isRunning do
                lock isRunning (fun () ->
                  if !isRunning then
                    let p = !percentValue
                    let t = (DateTime.UtcNow - start).TotalSeconds
                    let a = animation.[int t % animation.Length]
                    let percent =
                      if p=100 then [|'1';'0';'0';'%';' ';a|]
                      elif p<10 then [|' ';' ';char(48+p);'%';' ';a|]
                      else [|' ';char(48+p/10);char(48+p%10);'%';' ';a|]
                    eraseLine + textValue + String percent + eraseLine |> stdout.Write
                    Console.Out.Flush()
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
          eraseLine + String(' ', eraseLine.Length) + eraseLine + showCursor |> stdout.Write
          Console.Out.Flush()
    )

  let pause f =
    lock isRunning (fun () ->
      if !isRunning then
        stop(); f(); start() |> ignore
      else f()
    )

// TODO
// 123/199 tests
// test output, where can I be