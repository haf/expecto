/// A module for specifying what you expect from the values generated
/// by your tests.
module Expecto.Expect
[<assembly: System.Runtime.CompilerServices.InternalsVisibleTo("Expecto.BenchmarkDotNet")>]
()

open System
open Expecto.Logging
open Expecto.Logging.Message

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

/// Expects `actual` and `expected` (that are both floats) to be within a
/// given `accuracy`.
let floatClose accuracy actual expected format =
  if Accuracy.areClose accuracy actual expected then
    ()
  else
    Tests.failtestf
      "%s. Difference was %g but was expected to be less than %g to meet accuracy (absolute=%g relative=%g)"
      format (Accuracy.areCloseLhs actual expected)
             (Accuracy.areCloseRhs accuracy actual expected)
             accuracy.absolute accuracy.relative

/// Expects the two values to equal each other.
let inline equal (actual : 'a) (expected : 'a) format =
  match box actual, box expected with
  | (:? string as a), (:? string as e) ->
    use ai = a.GetEnumerator()
    use ei = e.GetEnumerator()
    let mutable i = 0
    let baseMsg errorIndex =
      let diffString = new String(' ', errorIndex + 1) + "↑"
      sprintf "%s.
          Expected string to equal:
          %A
          %s
          The string differs at index %d.
          %A
          %s"
                    format expected diffString errorIndex actual diffString
    while ei.MoveNext() do
      if ai.MoveNext() then
        if ai.Current = ei.Current then ()
        else
          Tests.failtestf "%s
          String does not match at position %i. Expected char: %A, but got %A."
            (baseMsg i) i ei.Current ai.Current
      else
        Tests.failtestf "%s
          String `actual` was shorter than expected, at pos %i for expected item %A."
          (baseMsg i) i ei.Current
      i <- i + 1
    if ai.MoveNext() then
      Tests.failtestf "%s
          String `actual` was longer than expected, at pos %i found item %A."
                      (baseMsg i) i (ai.Current)
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

let inline private formatSet<'a> (concatBy) (formatResult) (whenEmpty) (ss : 'a seq) : string =
  if Seq.isEmpty ss then
    whenEmpty
  else
    (match box (Seq.nth 0 ss) with
    | :? IComparable ->
      ss
      |> Seq.cast<IComparable>
      |> Seq.sort
      |> Seq.cast<'a>
      |> Seq.map (fun (a : 'a) -> a.ToString())
    | otherwise ->
      ss
      |> Seq.map (fun a -> a.ToString())
      |> Seq.sort)
    |> String.concat concatBy
    |> sprintf formatResult

/// Expects the `actual` sequence to contain all elements from `expected`
/// it doesn't take into account number of occurances of characters
/// sequence (not taking into account an order of elements). Calling this
/// function will enumerate both sequences; they have to be finite.
let containsAll (actual : _ seq)
                (expected : _ seq)
                format =
  let axs, exs = List.ofSeq actual, List.ofSeq expected
  let extra, missing =
    let ixs = axs |> List.filter (fun a -> exs |> List.exists ((=) a))
    axs |> List.filter (fun a -> not (ixs |> List.exists ((=) a))),
    exs |> List.filter (fun e -> not (ixs |> List.exists ((=) e)))

  if List.isEmpty missing then () else
  let formatResult = formatSet ", " "{%s}" "{}"
  sprintf "%s.
    Sequence `actual` does not contain all `expected` elements.
        All elements in `actual`:
        %s
        All elements in `expected`:
        %s
        Missing elements from `actual`:
        %s
        Extra elements in `actual`:
        %s"
              format (formatResult axs) (formatResult exs) (formatResult missing) (formatResult extra)
  |> Tests.failtest

let inline private except (elementsToCheck: Map<_,uint32>) (elementsToContain: Map<_,uint32>) (isExcept: bool) =
  let getMapValue (map: Map<_, uint32>) (element) =
    map |> Map.find element
  let getResult found expected =
    match isExcept with
    | true -> found, expected
    | _ -> expected, found

  let noOfFoundElements element value =
    let foundElements = (getMapValue elementsToContain element)
    match value > foundElements with
    | true -> getResult foundElements value
    | _ -> 0ul,0ul

  let elementsWhichDiffer element value =
    match elementsToContain |> Map.containsKey(element) with
    | true -> noOfFoundElements element value
    | _ -> getResult 0ul value

  let printResult found =
    sprintf "(%d/%d)" (fst found) (snd found)

  elementsToCheck
  |> Map.map elementsWhichDiffer
  |> Map.filter (fun key found -> snd found <> 0ul)
  |> Map.toList
  |> List.map (fun elem -> sprintf "'%A' %s" (fst elem) (snd elem |> printResult))

/// Expects the `actual` sequence to contain all elements from `expected` map,
/// first element in every tuple from `expected` map means item which should be
/// presented in `actual` sequence, the second element means an expected number of occurrences
/// of this item in sequence.
/// Function is not taking into account an order of elements.
/// Calling this function will enumerate both sequences; they have to be finite.
let distribution (actual : _ seq)
                (expected : Map<_,uint32>)
                format =
  let groupByOccurances sequence =
    sequence
    |> Seq.groupBy id
    |> Seq.map (fun (item, occurances) -> (item, occurances |> Seq.length |> uint32))
    |> Map.ofSeq
  let groupedActual = groupByOccurances actual

  let extra, missing =
    except groupedActual expected false,
    except expected groupedActual true

  let isCorrect = List.isEmpty missing && List.isEmpty extra
  if isCorrect then () else
  let formatInput(data) = formatSet ", " "{%s}" "" data
  let formatResult(data) = formatSet "\n\t" "%s" "" data
  let formatedExpected =
    expected
    |> Map.toList
    |> List.map (fun element -> sprintf "%A: %d" (fst element) (snd element))

  let missingElementsInfo, extraElementsInfo = 
    let incorrectElementsInfo elements is direction =
      if List.isEmpty elements then ""
      else sprintf "\n\t%s elements %s `actual`:\n\t%s" is direction (formatResult elements)
    incorrectElementsInfo missing "Missing" "from",
    incorrectElementsInfo extra "Extra" "in"

  sprintf "%s.
    Sequence `actual` does not contain every `expected` elements.
        All elements in `actual`:
        %s
        All elements in `expected` ['item', 'number of expected occurrences']:
        %s%s%s"
              format (formatInput actual) (formatInput formatedExpected) missingElementsInfo extraElementsInfo
  |> Tests.failtest

/// Expects the `actual` sequence to equal the `expected` one.
let sequenceEqual (actual : _ seq) (expected : _ seq) format =
  use ai = actual.GetEnumerator()
  use ei = expected.GetEnumerator()
  let baseMsg =
    sprintf "%s.
        Expected value was:
        %A
        Actual value was:
        %A"
                  format expected actual
  let mutable i = 0
  while ei.MoveNext() do
    if ai.MoveNext() then
      if ai.Current = ei.Current then ()
      else
        Tests.failtestf "%s
        Sequence does not match at position %i. Expected char: %A, but got %A."
                           baseMsg i (ei.Current) (ai.Current)
    else
      Tests.failtestf "%s
      Sequence actual shorter than expected, at pos %i for expected item %A."
                      baseMsg i (ei.Current)
    i <- i + 1
  if ai.MoveNext() then
    Tests.failtestf "%s
    Sequence actual longer than expected, at pos %i found item %A."
                      baseMsg i (ai.Current)

/// Expect the sequence `subject` to start with `prefix`. If it does not
/// then fail with `format` as an error message together with a description
/// of `subject` and `prefix`.
let sequenceStarts (subject : _ seq) (prefix : _ seq) format =
  use si = subject.GetEnumerator()
  use pi = prefix.GetEnumerator()
  let mutable i = 0
  while pi.MoveNext() do
    if si.MoveNext() then
      if si.Current = pi.Current then ()
      else
        Tests.failtestf "%s. Sequence does not match at position %i. Expected: %A, but got %A."
                           format i (pi.Current) (si.Current)
    else
      Tests.failtestf "%s. Sequence actual shorter than expected, at pos %i for expected item %A."
                      format i (pi.Current)
    i <- i + 1

/// Expect the sequence `subject` to be ascending. If it does not
/// then fail with `format` as an error message.
let isAscending (subject : _ seq) format =
  if not (subject |> Seq.windowed 2 |> Seq.forall (fun s -> s.[1] >= s.[0])) then
    Tests.failtestf "%s. Sequence is not ascending" format

/// Expect the sequence `subject` to be descending. If it does not
/// then fail with `format` as an error message.
let isDescending  (subject : _ seq) format =
  if not (subject |> Seq.windowed 2 |> Seq.forall (fun s -> s.[1] <= s.[0])) then
    Tests.failtestf "%s. Sequence is not descending" format

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

/// Expect the string `subject` to end with `suffix`. If it does not
/// then fail with `format` as an error message together with a description
/// of `subject` and `suffix`.
let stringEnds (subject : string) (suffix : string) format =
  if not (subject.EndsWith suffix) then
    Tests.failtestf "%s. Expected subject string '%s' to end with '%s'."
                    format subject suffix

/// Expect the string `subject` to have length equals `length`. If it does not
/// then fail with `format` as an error message together with a description
/// of `subject` and `length`.
let stringHasLength (subject : string) (length : int) format =
  if subject.Length <> length then
    Tests.failtestf "%s. Expected subject string '%s' to have length '%d'."
                    format subject length


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

/// Expects function `f1` is faster than `f2`. Measurer used to measure only a
/// subset of the functions. Statistical test to 99.99% confidence level.
let isFasterThanSub (f1:Performance.Measurer<_,_>->'a) (f2:Performance.Measurer<_,_>->'a) format =
  let toString (s:Performance.SampleStatistics) =
    sprintf "%.4f \u00B1 %.4f ms" s.mean s.meanStandardError

  match Performance.timeCompare f1 f2 with
  | Performance.ResultNotTheSame (r1, r2)->
    Tests.failtestf "%s. Expected function results to be the same (%A vs %A)."
                    format r1 r2
  | Performance.MetricTooShort (s,p) ->
    Tests.failtestf "%s. Expected metric (%s) to be much longer than the machine resolution (%s)."
                    format (toString s) (toString p)
  | Performance.MetricEqual (s1,s2) ->
    Tests.failtestf "%s. Expected f1 (%s) to be faster than f2 (%s) but are equal."
                    format (toString s1) (toString s2)
  | Performance.MetricMoreThan (s1,s2) ->
    Tests.failtestf "%s. Expected f1 (%s) to be faster than f2 (%s) but is ~%.0f%% slower."
                    format (toString s1) (toString s2) ((s1.mean/s2.mean-1.0)*100.0)
  | Performance.MetricLessThan (s1,s2) ->
    Impl.logger.info (
      eventX "{message}. f1 ({sample1}) is {percent} faster than f2 ({sample2})."
      >> setField "message" format
      >> setField "sample1" (toString s1)
      >> setField "percent" (sprintf "~%.0f%%" ((1.0-s1.mean/s2.mean)*100.0))
      >> setField "sample2" (toString s2))
    //printfn "%s. f1 (%s) is ~%.0f%% faster than f2 (%s)." format (toString s1) ((1.0-s1.mean/s2.mean)*100.0) (toString s2)

/// Expects function `f1` is faster than `f2`. Statistical test to 99.99%
/// confidence level.
let isFasterThan (f1:unit->'a) (f2:unit->'a) format =
  isFasterThanSub (fun measurer -> measurer f1 ())
                  (fun measurer -> measurer f2 ())
                  format