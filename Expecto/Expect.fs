module Expecto.Expect

open System

let throws f format =
  let msg = Printf.ksprintf (fun msg -> msg) format
  try
    f ()
    Tests.failtestf "%s. Expected f to throw." msg
  with e ->
    ()

let throwsC f cont =
  try
    f ()
    Tests.failtest "Expected f to throw."
  with e ->
    cont e

let throwsT<'texn> f format =
  let msg = Printf.ksprintf (fun msg -> msg) format
  try
    f ()
    Tests.failtestf "%s. Expected f to throw." msg
  with
  | e when e.GetType() <> typeof<'texn> ->
    Tests.failtestf "%s. Expected f to throw an exn of type %s."
                    msg
                    (typeof<'texn>.Name)
  | e ->
    ()


let isNone x format =
  match x with
  | None -> ()
  | Some x ->
    let msg = Printf.ksprintf (fun msg -> msg) format
    Tests.failtestf "%s. Expected None, was Some(%A)."
                    msg x

let isSome x format =
  match x with
  | None ->
    let msg = Printf.ksprintf (fun msg -> msg) format
    Tests.failtestf "%s. Expected Some _, was None." msg
  | Some _ ->
    ()

let isChoice1Of2 x format =
  match x with
  | Choice1Of2 _ -> ()
  | Choice2Of2 x ->
    let msg = Printf.ksprintf (fun msg -> msg) format
    Tests.failtestf "%s. Expected Choice1Of2, was Choice2Of2(%A)." msg x

let isChoice2Of2 x format =
  match x with
  | Choice1Of2 x ->
    let msg = Printf.ksprintf (fun msg -> msg) format
    Tests.failtestf "%s. Expected Choice2Of2 _, was Choice1Of2(%A)."
                    msg x
  | Choice2Of2 _ ->
    ()

let isNotNull x format =
  match x with
  | null ->
    let msg = Printf.ksprintf (fun msg -> msg) format
    Tests.failtest msg
  | x -> ()

let isNull x format =
  match x with
  | null -> ()
  | x ->
    let msg = Printf.ksprintf (fun msg -> msg) format
    Tests.failtestf "%s. Expected null, but was %A."
                    msg x

let isLessThan a b format =
  if a < b then ()
  else
    let msg = Printf.ksprintf (fun msg -> msg) format
    Tests.failtestf "%s. Expected a (%A) to be less than b (%A)."
                    msg a b

let isLessThanOrEqual a b format =
  if a <= b then ()
  else
    let msg = Printf.ksprintf (fun msg -> msg) format
    Tests.failtestf "%s. Expected a (%A) to be less than or equal to b (%A)."
                    msg a b

let isGreaterThan a b format =
  if a > b then ()
  else
    let msg = Printf.ksprintf (fun msg -> msg) format
    Tests.failtestf "%s. Expected a (%A) to be greater than or equal to b (%A)."
                    msg a b

let isGreaterThanOrEqual a b format =
  if a >= b then ()
  else
    let msg = Printf.ksprintf (fun msg -> msg) format
    Tests.failtestf "%s. Expected a (%A) to be greater than or equal to b (%A)"
                    msg a b

  /// specify two floats equal within a given error - epsilon.
let floatEqual actual expected epsilon format =
  let epsilon = defaultArg epsilon 0.001
  if expected <= actual + epsilon && expected >= actual - epsilon then
    ()
  else
    let msg = Printf.ksprintf (fun msg -> msg) format
    Tests.failtestf "%s. Actual value was %f but was expected to be %f within %f epsilon."
                    msg actual expected epsilon

let equal (actual : 'a) (expected : 'a) format =
  if expected = actual then ()
  else
    let msg = Printf.ksprintf id format
    Tests.failtestf "%s. Actual value was %A but had expected it to be %A."
                    msg actual expected msg

let notEqual (actual : 'a) (expected : 'a) format =
  if expected <> actual then ()
  else
    let msg = Printf.ksprintf (fun msg -> msg) format
    Tests.failtestf "%s. Actual value was equal to %A but had expected it non-equal."
                    msg actual

let isFalse actual format =
  if not actual then ()
  else
    Printf.ksprintf Tests.failtest format

let isTrue actual format =
  if actual then ()
  else
    Printf.ksprintf Tests.failtest format

let contains sequence element format =
  match sequence |> Seq.tryFind ((=) element) with
  | Some _ -> ()
  | None ->
    let msg = Printf.ksprintf (fun msg -> msg) format
    Tests.failtestf "%s. Sequence did not contain %A." msg element

let sequenceEqual (actual : _ seq) (expected : _ seq) format =
  use ai = actual.GetEnumerator()
  use ei = expected.GetEnumerator()
  let mutable i = 0
  while ei.MoveNext() do
    if ai.MoveNext() then
      if ai.Current = ei.Current then ()
      else
        let msg = Printf.ksprintf (fun msg -> msg) format
        Tests.failtestf "%s. Sequence do not match at position %i. Expected: %A, but got %A."
                           msg i (ei.Current) (ai.Current)
    else
      let msg = Printf.ksprintf (fun msg -> msg) format
      Tests.failtestf "%s. Sequence actual shorter than expected, at pos %i for expected item %A."
                      msg i (ei.Current)
    i <- i + 1

/// Ensures that the subject string contains the given substring. Otherwise
/// fails with the passed message.
let stringContains (subject : string) (substring : string) format =
  if not (subject.Contains(substring)) then
    let msg = Printf.ksprintf (fun msg -> msg) format
    Tests.failtestf "%s. Expected subject string '%s' to contain substring '%s'."
                    msg subject substring
