namespace Expecto
open Expecto
open Hopac
open System.Runtime.CompilerServices

[<AutoOpen; Extension>]
module Tests =
  /// Builds an job test case
  let inline testCaseJob name (test : #Job<unit>) = TestLabel(name, TestCase (Async (test |> Job.toAsync),Normal), Normal)
  /// Builds an job test case that will make Expecto to ignore other unfocused tests
  let inline ftestCaseJob name (test : #Job<unit>) = TestLabel(name, TestCase (Async (test |> Job.toAsync), Focused), Focused)
  /// Builds an job test case that will be ignored by Expecto
  let inline ptestCaseJob name (test : #Job<unit>) = TestLabel(name, TestCase (Async (test |> Job.toAsync), Pending), Pending)

  type TestJobBuilder(name, focusState) =
    member __.Zero() = job.Zero()
    member __.Delay(f) = Job.delay f
    member __.Return(x) = job.Return(x)
    member __.ReturnFrom(x : Job<_>) = job.ReturnFrom(x)
    member __.Bind(p1 : Job<_>, p2) = job.Bind(p1, p2)
    member __.Using(g, p) = job.Using(g, p)
    member __.While(gd, prog) = job.While(gd, prog)
    member __.For(e, prog) = job.For(e, prog)
    member __.Combine(p1, p2) = job.Combine(p1, p2)
    member __.TryFinally(p, cf) = job.TryFinally(p, cf)
    member __.TryWith(p, cf) = job.TryWith(p, cf)
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
