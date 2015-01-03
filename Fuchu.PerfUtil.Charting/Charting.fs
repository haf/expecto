module Fuchu.PerfUtil.Charting

open PerfUtil

open FSharp.Charting

open System
open System.Drawing
open System.Windows.Forms.DataVisualization.Charting

// you can also plot them in an interactive:
let dashGrid = ChartTypes.Grid(LineColor = Color.Gainsboro, LineDashStyle = ChartDashStyle.Dash)

let plot yaxis (metric : PerfResult -> float) (results : PerfResult list) =
    let values = results |> List.choose (fun r -> if r.HasFailed then None else Some (r.SessionId, metric r))
    let name = results |> List.tryPick (fun r -> let i = r.TestId.IndexOf('.') in Some <| r.TestId.[i+1..])
    Chart.Bar(values, ?Name = name, ?Title = name, YTitle = yaxis)
    |> Chart.WithYAxis(MajorGrid = dashGrid)
    |> Chart.WithXAxis(MajorGrid = dashGrid)
    |> fun ch -> ch.ShowChart()

let plotMS (results : TestSession list) = 
    results 
    |> TestSession.groupByTest
    |> Map.iter (fun _ rs -> plot "milliseconds" (fun r -> r.Elapsed.TotalMilliseconds) rs)

// TODO: nice methods for charting it
//PerfTest.run mkTester normal_serlialisation |> plotMS
