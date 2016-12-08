/// A module for specifying what you expect from the values generated
/// by your tests.
module Expecto.Expect
[<assembly: System.Runtime.CompilerServices.InternalsVisibleTo("Expecto.BenchmarkDotNet")>]
()

open System

/// Expects f to throw an exception.
let throws f format =
  try
    f ()
    Tests.failtestf "%s. Expected f to throw." format
  with e ->
    ()

/// Expects f to throw, and calls `cont` with its exception.
let throwsC f cont =
  try
    f ()
    Tests.failtest "Expected f to throw."
  with e ->
    cont e

/// Expects the passed function to throw `'texn`.
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


/// Expects the value to be a None value.
let isNone x format =
  match x with
  | None ->
    ()
  | Some x ->
    Tests.failtestf "%s. Expected None, was Some(%A)."
                    format x

/// Expects the value to be a Some _ value.
let isSome x format =
  match x with
  | None ->
    Tests.failtestf "%s. Expected Some _, was None." format
  | Some _ ->
    ()

/// Expects the value to be a Choice1Of2 value.
let isChoice1Of2 x format =
  match x with
  | Choice1Of2 _ -> ()
  | Choice2Of2 x ->
    Tests.failtestf "%s. Expected Choice1Of2, was Choice2Of2(%A)." format x

/// Expects the value to be a Choice2Of2 value.
let isChoice2Of2 x format =
  match x with
  | Choice1Of2 x ->
    Tests.failtestf "%s. Expected Choice2Of2 _, was Choice1Of2(%A)."
                    format x
  | Choice2Of2 _ ->
    ()

/// Expects the value not to be null.
let isNotNull x format =
  match x with
  | null ->
    Tests.failtest format
  | x ->
    ()

/// Expects the value to be null.
let isNull x format =
  match x with
  | null -> ()
  | x ->
    Tests.failtestf "%s. Expected null, but was %A."
                    format x

/// Expects `a` to be less than `b`.
let isLessThan a b format =
  if a >= b then
    Tests.failtestf "%s. Expected a (%A) to be less than b (%A)."
                    format a b

/// Expects `a` <= `b`.
let isLessThanOrEqual a b format =
  if a > b then
    Tests.failtestf "%s. Expected a (%A) to be less than or equal to b (%A)."
                    format a b

/// Expects `a` > `b`.
let isGreaterThan a b format =
  if a > b then ()
  else
    Tests.failtestf "%s. Expected a (%A) to be greater than or equal to b (%A)."
                    format a b

/// Expects `a` >= `b`.
let isGreaterThanOrEqual a b format =
  if a >= b then ()
  else
    Tests.failtestf "%s. Expected a (%A) to be greater than or equal to b (%A)"
                    format a b

/// Expects `actual` and `expected` (that are both floats) to equal within a
/// given `epsilon`.
let floatEqual actual expected epsilon format =
  let epsilon = defaultArg epsilon 0.001
  if expected <= actual + epsilon && expected >= actual - epsilon then
    ()
  else
    Tests.failtestf "%s. Actual value was %f but was expected to be %f within %f epsilon."
                    format actual expected epsilon

/// Expects the two values to equal each other.
let inline equal (actual : 'a) (expected : 'a) format =
  match box actual, box expected with
  | (:? string as a), (:? string as e) ->
    use ai = a.GetEnumerator()
    use ei = e.GetEnumerator()
    let mutable i = 0
    while ei.MoveNext() do
      if ai.MoveNext() then
        if ai.Current = ei.Current then ()
        else
          Tests.failtestf "%s. String do not match at position %i. Expected: '%A', but got '%A'."
                            format i (ei.Current) (ai.Current)
      else
        Tests.failtestf "%s. String actual shorter than expected, at pos %i for expected '%A'."
                        format i (ei.Current)
      i <- i + 1
  | _, _ ->
    if actual <> expected then
      Tests.failtestf "%s. Actual value was %A but had expected it to be %A." format actual expected



/// Expects the two values not to equal each other.
let notEqual (actual : 'a) (expected : 'a) format =
  if expected = actual then
    Tests.failtestf "%s. Actual value was equal to %A but had expected it non-equal."
                    format actual

/// Expects the value to be false.
let isFalse actual format =
  if not actual then ()
  else
    Tests.failtest format

/// Expects the value to be true.
let isTrue actual format =
  if actual then ()
  else
    Tests.failtest format

/// Expects the `sequence` to contain the `element`.
let contains sequence element format =
  match sequence |> Seq.tryFind ((=) element) with
  | Some _ -> ()
  | None ->
    Tests.failtestf "%s. Sequence did not contain %A." format element

/// Expects the `actual` sequence to equal the `expected` one.
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

/// Expect the string `subject` to contain `substring` as part of itself.
/// If it does not, then fail with `format` and `subject` and `substring`
/// as part of the error message.
let stringContains (subject : string) (substring : string) format =
  if not (subject.Contains(substring)) then
    Tests.failtestf "%s. Expected subject string '%s' to contain substring '%s'."
                    format subject substring

/// Expect the string `subject` to start with `prefix`. If it does not
/// then fail with `format` as an error message together with a description
/// of `subject` and `prefix`.
let stringStarts (subject : string) (prefix : string) format =
  if not (subject.StartsWith prefix) then
    Tests.failtestf "%s. Expected subject string '%s' to start with '%s'."
                    format subject prefix


/// Expect the streams to byte-wise equal.
let streamsEqual (s1 : IO.Stream) (s2 : IO.Stream) format =
    let buf = Array.zeroCreate<byte> 2
    let rec compare pos =
      match s1.Read(buf, 0, 1), s2.Read(buf, 1, 1) with
      | x, y when x <> y ->
        Tests.failtestf "%s. Not equal at pos %d" format pos
      | 0, _ ->
        ()
      | _ when buf.[0] <> buf.[1] ->
        Tests.failtestf "%s. Not equal at pos %d" format pos
      | _ ->
        compare (pos + 1)
    compare 0
