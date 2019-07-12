namespace Expecto
open Expecto
open Hopac
open System
open System.Runtime.CompilerServices
open System.Threading.Tasks

[<AutoOpen; Extension>]
module Tests =
  /// Builds an job test case
  let inline testCaseJob name (test : #Job<unit>) = TestLabel(name, TestCase (Async (test |> Job.toAsync),Normal), Normal)
  /// Builds an job test case that will make Expecto to ignore other unfocused tests
  let inline ftestCaseJob name (test : #Job<unit>) = TestLabel(name, TestCase (Async (test |> Job.toAsync), Focused), Focused)
  /// Builds an job test case that will be ignored by Expecto
  let inline ptestCaseJob name (test : #Job<unit>) = TestLabel(name, TestCase (Async (test |> Job.toAsync), Pending), Pending)

  type TestJobBuilder(name, focusState) =
    member inline __.Zero() = job.Zero()
    member inline __.Delay(f) = Job.delay f
    member inline __.Return(x) = job.Return(x)
    member inline __.ReturnFrom (x: IObservable<'x>) =job.ReturnFrom(x)
    member inline __.ReturnFrom (x: Async<'x>) =job.ReturnFrom(x)
    member inline __.ReturnFrom (x: Task<'x>) =job.ReturnFrom(x)
    member inline __.ReturnFrom(x : Job<_>) = job.ReturnFrom(x)
    member inline __.Bind (p1: IObservable<'x>, p2) = job.Bind(p1, p2)
    member inline __.Bind (p1: Async<'x>, p2) = job.Bind(p1,p2)
    member inline __.Bind (p1: Task<'x>, p2) = job.Bind(p1,p2)
    member inline __.Bind(p1 : Job<_>, p2) = job.Bind(p1, p2)
    member inline __.Using(g, p) = job.Using(g, p)
    member inline __.While(gd, prog) = job.While(gd, prog)
    member inline __.For(e, prog) = job.For(e, prog)
    member inline __.Combine(p1, p2) = job.Combine(p1, p2)
    member inline __.TryFinally(p, cf) = job.TryFinally(p, cf)
    member inline __.TryWith(p, cf) = job.TryWith(p, cf)
    member __.Run f =
      match focusState with
      | Normal -> testCaseJob name f
      | Focused -> ftestCaseJob name f
      | Pending -> ptestCaseJob name f

  /// Builds an job test case
  let inline testJob name =
    TestJobBuilder (name, Normal)
  /// Builds an job test case that will make Expecto to ignore other unfocused tests
  let inline ftestJob name =
    TestJobBuilder (name, Focused)
  /// Builds an job test case that will be ignored by Expecto
  let inline ptestJob name =
    TestJobBuilder (name, Pending)
