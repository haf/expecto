module Expecto.Expect

open System

let throws f format =
  try
    f ()
    Tests.failtestf "%s. Expected f to throw." format
  with e ->
    ()

let throwsC f cont =
  try
    f ()
    Tests.failtest "Expected f to throw."
  with e ->
    cont e

let throwsT<'texn> f format =
  try
    f ()
    Tests.failtestf "%s. Expected f to throw." format
  with
  | e when e.GetType() <> typeof<'texn> ->
    Tests.failtestf "%s. Expected f to throw an exn of type %s."
                    format
                    (typeof<'texn>.Name)
  | e ->
    ()


let isNone x format =
  match x with
  | None ->
    ()
  | Some x ->
    Tests.failtestf "%s. Expected None, was Some(%A)."
                    format x

let isSome x format =
  match x with
  | None ->
    Tests.failtestf "%s. Expected Some _, was None." format
  | Some _ ->
    ()

let isChoice1Of2 x format =
  match x with
  | Choice1Of2 _ -> ()
  | Choice2Of2 x ->
    Tests.failtestf "%s. Expected Choice1Of2, was Choice2Of2(%A)." format x

let isChoice2Of2 x format =
  match x with
  | Choice1Of2 x ->
    Tests.failtestf "%s. Expected Choice2Of2 _, was Choice1Of2(%A)."
                    format x
  | Choice2Of2 _ ->
    ()

let isNotNull x format =
  match x with
  | null ->
    Tests.failtest format
  | x ->
    ()

let isNull x format =
  match x with
  | null -> ()
  | x ->
    Tests.failtestf "%s. Expected null, but was %A."
                    format x

let isLessThan a b format =
  if a >= b then
    Tests.failtestf "%s. Expected a (%A) to be less than b (%A)."
                    format a b

let isLessThanOrEqual a b format =
  if a > b then
    Tests.failtestf "%s. Expected a (%A) to be less than or equal to b (%A)."
                    format a b

let isGreaterThan a b format =
  if a > b then ()
  else
    Tests.failtestf "%s. Expected a (%A) to be greater than or equal to b (%A)."
                    format a b

let isGreaterThanOrEqual a b format =
  if a >= b then ()
  else
    Tests.failtestf "%s. Expected a (%A) to be greater than or equal to b (%A)"
                    format a b

  /// specify two floats equal within a given error - epsilon.
let floatEqual actual expected epsilon format =
  let epsilon = defaultArg epsilon 0.001
  if expected <= actual + epsilon && expected >= actual - epsilon then
    ()
  else
    Tests.failtestf "%s. Actual value was %f but was expected to be %f within %f epsilon."
                    format actual expected epsilon

let inline equal (actual : 'a) (expected : 'a) format =
  if expected <> actual then
    Tests.failtestf "%s. Actual value was %A but had expected it to be %A."
                    format actual expected

let notEqual (actual : 'a) (expected : 'a) format =
  if expected = actual then
    Tests.failtestf "%s. Actual value was equal to %A but had expected it non-equal."
                    format actual

let isFalse actual format =
  if not actual then ()
  else
    Tests.failtest format

let isTrue actual format =
  if actual then ()
  else
    Tests.failtest format

let contains sequence element format =
  match sequence |> Seq.tryFind ((=) element) with
  | Some _ -> ()
  | None ->
    Tests.failtestf "%s. Sequence did not contain %A." format element

let sequenceEqual (actual : _ seq) (expected : _ seq) format =
  use ai = actual.GetEnumerator()
  use ei = expected.GetEnumerator()
  let mutable i = 0
  while ei.MoveNext() do
    if ai.MoveNext() then
      if ai.Current = ei.Current then ()
      else
        Tests.failtestf "%s. Sequence do not match at position %i. Expected: %A, but got %A."
                           format i (ei.Current) (ai.Current)
    else
      Tests.failtestf "%s. Sequence actual shorter than expected, at pos %i for expected item %A."
                      format i (ei.Current)
    i <- i + 1

/// Ensures that the subject string contains the given substring. Otherwise
/// fails with the passed message.
let stringContains (subject : string) (substring : string) format =
  if not (subject.Contains(substring)) then
    Tests.failtestf "%s. Expected subject string '%s' to contain substring '%s'."
                    format subject substring
