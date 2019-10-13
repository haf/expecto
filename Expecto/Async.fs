module internal Expecto.Async

open System
open System.Threading
open System.Threading.Tasks

let map fn a =
  async {
    let! v = a
    return fn v
  }

let bind fn a =
  async.Bind(a, fn)

let foldSequentiallyWithCancel (ct: CancellationToken) folder state (s:_ seq) =
  async {
    let mutable state = state
    let tcs = TaskCompletionSource()
    use _ = Action tcs.SetResult |> ct.Register
    let tasks: Task[] = [| null; tcs.Task |]
    use e = s.GetEnumerator()
    while not ct.IsCancellationRequested && e.MoveNext() do
      let task = Async.StartAsTask e.Current
      tasks.[0] <- task :> Task
      if Task.WaitAny tasks = 0 then
        state <- folder state task.Result
    return state
  }

let foldSequentially folder state (s: _ seq) =
  foldSequentiallyWithCancel CancellationToken.None folder state s

let foldParallelWithCancel maxParallelism (ct: CancellationToken) folder state (s: _ seq) =
  async {
    let mutable state = state
    use e = s.GetEnumerator()
    if e.MoveNext() then
      let mutable tasks = [Async.StartAsTask e.Current]
      while not(ct.IsCancellationRequested || List.isEmpty tasks) do
        if List.length tasks = maxParallelism || not(e.MoveNext()) then
          while not( tasks |> List.exists (fun t -> t.IsCompleted)
                  || ct.IsCancellationRequested) do
            do! Async.Sleep 10
          tasks |> List.tryFindIndex (fun t -> t.IsCompleted)
          |> Option.iter (fun i ->
            let a,b = List.splitAt i tasks
            state <- (List.head b).Result |> folder state
            tasks <- a @ List.tail b
          )
        else tasks <- Async.StartAsTask e.Current :: tasks
    return state
  }


