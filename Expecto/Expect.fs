module Expecto.Expect

open System

let throws f =
  try
    f ()
    Tests.failtest "Expected f to throw"
  with e ->
    ()

let throwsc f cont =
  try
    f ()
    Tests.failtest "Expected f to throw"
  with e ->
    cont e

let throwsT<'texn> f : unit =
  try
    f ()
    Tests.failtest "Expected f to throw"
  with
  | e when e.GetType() <> typeof<'texn> ->
    Tests.failtestf "Expected f to throw of type %s" (typeof<'texn>.Name)
  | e ->
    ()


let isNone x msg =
  match x with
  | None -> ()
  | Some x -> Tests.failtestf "Expected None, was Some(%A). %s" x msg

let isNonef value format =
  match value with
  | None -> ()
  | Some x -> Printf.kprintf Tests.failtest format x

let isSome x msg =
  match x with
  | None -> Tests.failtestf "Expected Some _, was None. %s" msg
  | Some _ -> ()

let isChoice1Of2 = function
  | Choice1Of2 _ -> ()
  | Choice2Of2 x -> Tests.failtestf "Expected Choice1Of2, was Choice2Of2(%A)" x

let isChoice1Of2f value format =
  match value with
  | Choice1Of2 _ -> ()
  | Choice2Of2 x -> Printf.kprintf Tests.failtest format x

let isChoice2Of2 = function
  | Choice1Of2 x -> Tests.failtestf "Expected Choice2Of2 _, was Choice1Of2(%A)" x
  | Choice2Of2 _ -> ()

let isChoice2Of2f value format =
  match value with
  | Choice1Of2 x -> Printf.kprintf Tests.failtest format x
  | Choice2Of2 _ -> ()

let isNotNull x format =
  match x with
  | null -> Tests.failtestf format
  | x -> ()

let isNull = function
  | null -> ()
  | x -> Tests.failtestf "Expected null, but was %A" x

let isNullf value format =
  match value with
  | null -> ()
  | x -> Printf.kprintf Tests.failtest format x

let isLessThan a b msg =
  if a < b then ()
  else Tests.failtestf "Expected a (%A) to be less than b (%A)" a b

let isLessThanOrEqual a b msg =
  if a <= b then ()
  else Tests.failtestf "Expected a (%A) to be less than or equal to b (%A)" a b

let isGreaterThan a b msg =
  if a > b then ()
  else Tests.failtestf "Expected a (%A) to be greater than or equal to b (%A)" a b

let isGreaterThanOrEqual a b msg =
  if a >= b then ()
  else Tests.failtestf "Expected a (%A) to be greater than or equal to b (%A)" a b

  /// specify two floats equal within a given error - epsilon.
let floatEqual actual expected epsilon msg =
  let epsilon = defaultArg epsilon 0.001
  if expected <= actual + epsilon && expected >= actual - epsilon then
    ()
  else
    Tests.failtestf "Actual value was %f but was expected to be %f within %f epsilon. %s"
                    actual expected epsilon msg

let equal (actual : 'a) (expected : 'a) (msg : string) =
  if expected = actual then ()
  else Tests.failtestf "Actual value was %A but had expected it to be: %A. %s"
                       actual expected msg

let notEqual (actual : 'a) (expected : 'a) (msg : string) =
  if expected <> actual then ()
  else Tests.failtestf "Actual value was equal to %A but had expected it non-equal. %s"
                       actual msg

let isFalse actual msg =
  if not actual then ()
  else Tests.failtest msg

let isTrue actual msg =
  if actual then ()
  else Tests.failtest msg

let sequenceEqual (actual : _ seq) (expected : _ seq) msg =
  use ai = actual.GetEnumerator()
  use ei = expected.GetEnumerator()
  let mutable i = 0
  while ei.MoveNext() do
    if ai.MoveNext() then
      if ai.Current = ei.Current then ()
      else Tests.failtestf "%s. Sequence do not match at position %i. Expected: %A, but got %A"
                           msg i (ei.Current) (ai.Current)
    else
      Tests.failtestf "%s. Sequence actual shorter than expected, at pos %i for expected item %A"
                      msg i (ei.Current)
    i <- i + 1

/// Ensures that the subject string contains the given substring. Otherwise
/// fails with the passed message.
let stringContains (subject : string) (substring : string) (message : string) =
  if not (subject.Contains(substring)) then
    Tests.failtestf "Expected subject string '%s' to contain substring '%s'. %s"
                    subject substring message
