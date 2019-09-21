/// A module for specifying what you expect from the values generated
/// by your tests.
module Expecto.Expect
[<assembly: System.Runtime.CompilerServices.InternalsVisibleTo("Expecto.BenchmarkDotNet")>]
()

open DiffPlex
open DiffPlex.DiffBuilder
open DiffPlex.DiffBuilder.Model
open System
open System.Text.RegularExpressions
open Expecto.Logging
open Expecto.Logging.Message
open Microsoft.FSharp.Reflection
open System.Reflection

let private isNull' value = isNull value

let private firstDiff s1 s2 =
  let s1 = Seq.append (Seq.map Some s1) (Seq.initInfinite (fun _ -> None))
  let s2 = Seq.append (Seq.map Some s2) (Seq.initInfinite (fun _ -> None))
  Seq.mapi2 (fun i s p -> i,s,p) s1 s2
  |> Seq.find (function |_,Some s,Some p when s=p -> false |_-> true)

let private allDiffs (s1:string) (s2:string) =
  let q1 = Seq.append (Seq.map Some s1) (Seq.initInfinite (fun _ -> None))
  let q2 = Seq.append (Seq.map Some s2) (Seq.initInfinite (fun _ -> None))
  Seq.mapi2 (fun i s p -> i,s,p) q1 q2
  |> Seq.take (1+max s1.Length s2.Length)
  |> Seq.fold (fun (onFrom,l) v ->
      match onFrom, v with
      | None,(i,f,s) when f<>s -> Some i, l
      | Some i,(j,f,s) when f=s -> None, (i,j)::l
      | onFrom,_ -> onFrom, l
  ) (None,[])
  |> snd

let private highlightAllRed diffs (s:string) =
  let redStart = ANSIOutputWriter.colourText (ANSIOutputWriter.getColour()) ConsoleColor.Red
  let redEnd = ANSIOutputWriter.colourText (ANSIOutputWriter.getColour()) ConsoleColor.Cyan
  let l = s.Length
  List.fold (fun (s:string) (i,j) ->
    if i>=l then s
    else s.Insert(min j l,redEnd).Insert(i,redStart)
  ) s diffs

let private highlightAllGreen diffs (s:string) =
  let greenStart = ANSIOutputWriter.colourText (ANSIOutputWriter.getColour()) ConsoleColor.Green
  let greenEnd = ANSIOutputWriter.colourText (ANSIOutputWriter.getColour()) ConsoleColor.Cyan
  let l = s.Length
  List.fold (fun (s:string) (i,j) ->
    if i>=l then s
    else s.Insert(min j l,greenEnd).Insert(i,greenStart)
  ) s diffs

let private printVerses (firstName:string) first (secondName:string) second =
  let first, second =
    match box first, box second with
    | (:? string as f), (:? string as s) ->
      string f, string s
    | f, s ->
      sprintf "%A" f, sprintf "%A" s

  let differ = SideBySideDiffBuilder(Differ())
  let diff = differ.BuildDiffModel(first, second)

  let coloredText typ text =
    let colorSprintf color =
      sprintf "%s%s%s"
        (ANSIOutputWriter.colourText (ANSIOutputWriter.getColour()) color)
        text
        ANSIOutputWriter.colourReset
    match typ with
    | ChangeType.Inserted -> colorSprintf ConsoleColor.Green
    | ChangeType.Deleted -> colorSprintf ConsoleColor.Red
    | ChangeType.Modified -> colorSprintf ConsoleColor.Blue
    | ChangeType.Unchanged | ChangeType.Imaginary | _ -> text

  let colorizedDiff (lines: DiffPiece seq) =
    lines
    |> Seq.map (fun line ->
      if line.SubPieces.Count = 0 then
        coloredText line.Type line.Text
      else
        let coloredPieces = line.SubPieces |> Seq.map (fun piece -> coloredText piece.Type piece.Text)
        coloredPieces |> fun x -> String.Join("", x)
      )
    |> fun x -> String.Join("\n", x)

  sprintf "%s\n---------- Actual: --------------------\n%s\n---------- Expected: ------------------\n%s\n" ANSIOutputWriter.colourReset (colorizedDiff diff.OldText.Lines) (colorizedDiff diff.NewText.Lines)

/// Expects f to throw an exception.
let throws f message =
  let thrown =
    try
      f ()
      false
    with _ ->
      true

  if not thrown then failtestf "%s. Expected f to throw." message

/// Expects f to throw, and calls `cont` with its exception.
let throwsC f cont =
  let thrown =
    try
      f ()
      None
    with e ->
      Some e

  match thrown with
  | Some e -> cont e
  | _ -> failtestf "Expected f to throw."

/// Expects the passed function to throw `'texn`.
let throwsT<'texn> f message =
  let thrown =
    try
      f ()
      None
    with e ->
      Some e
  match thrown with
  | Some e when e.GetType() <> typeof<'texn> ->
    failtestf "%s. Expected f to throw an exn of type %s, but one of type %s was thrown."
      message
      (typeof<'texn>.FullName)
      (e.GetType().FullName)
  | Some _ -> ()
  | _ -> failtestf "%s. Expected f to throw." message

/// Expects the value to be a None value.
let isNone x message =
  match x with
  | None ->
    ()
  | Some x ->
    failtestf "%s. Expected None, was Some(%A)."
      message x

/// Expects the value to be a Some _ value.
let isSome x message =
  match x with
  | None ->
    failtestf "%s. Expected Some _, was None." message
  | Some _ ->
    ()

/// Expects the value to be a Choice1Of2 value.
let isChoice1Of2 x message =
  match x with
  | Choice1Of2 _ -> ()
  | Choice2Of2 x ->
    failtestf "%s. Expected Choice1Of2, was Choice2Of2(%A)." message x

/// Expects the value to be a Choice2Of2 value.
let isChoice2Of2 x message =
  match x with
  | Choice1Of2 x ->
    failtestf "%s. Expected Choice2Of2 _, was Choice1Of2(%A)."
      message x
  | Choice2Of2 _ ->
    ()

/// Expects the value to be a Result.Ok value.
let isOk x message =
  match x with
  | Ok _ -> ()
  | Result.Error x ->
    failtestf "%s. Expected Ok, was Error(%A)." message x

/// Expects the value to be a Result.Error value.
let isError x message =
  match x with
  | Ok x ->
    failtestf "%s. Expected Error _, was Ok(%A)." message x
  | Result.Error _ -> ()

/// Expects the value not to be null.
let isNotNull x message =
  match x with
  | null ->
    failtest message
  | x ->
    ()

/// Expects the value to be null.
let isNull x message =
  match x with
  | null -> ()
  | x ->
    failtestf "%s. Expected null, but was %A."
      message x

/// Expects `a` to be less than `b`.
let isLessThan a b message =
  if a >= b then
    failtestf "%s. Expected a (%A) to be less than b (%A)."
      message a b

/// Expects `a` <= `b`.
let isLessThanOrEqual a b message =
  if a > b then
    failtestf "%s. Expected a (%A) to be less than or equal to b (%A)."
      message a b

/// Expects `a` > `b`.
let isGreaterThan a b message =
  if a > b then ()
  else
    failtestf "%s. Expected a (%A) to be greater than b (%A)."
      message a b

/// Expects `a` >= `b`.
let isGreaterThanOrEqual a b message =
  if a >= b then ()
  else
    failtestf "%s. Expected a (%A) to be greater than or equal to b (%A)."
      message a b

/// Expects `actual` and `expected` (that are both floats) to equal within a
/// given `epsilon`.
[<Obsolete "Please use the more general Expect.floatClose">]
let floatEqual actual expected epsilon message =
  let epsilon = defaultArg epsilon 0.001
  if expected <= actual + epsilon && expected >= actual - epsilon then
    ()
  else
    failtestf "%s. Actual value was %f but was expected to be %f within %f epsilon."
      message actual expected epsilon

/// Expects `actual` and `expected` (that are both floats) to be within a
/// given `accuracy`.
let floatClose accuracy actual expected message =
  if Double.IsInfinity actual then
    failtestf "%s. Expected actual to not be infinity, but it was." message
  elif Double.IsInfinity expected then
    failtestf "%s. Expected expected to not be infinity, but it was." message
  elif Accuracy.areClose accuracy actual expected |> not then
    failtestf
      "%s. Expected difference to be less than %f for accuracy {absolute=%f; relative=%f}, but was %f. actual=%f expected=%f"
      message (Accuracy.areCloseRhs accuracy actual expected)
      accuracy.absolute accuracy.relative
      (Accuracy.areCloseLhs actual expected)
      actual expected
/// Expects `actual` to be less than `expected` or to be within a
/// given `accuracy`.
let floatLessThanOrClose accuracy actual expected message =
    if actual>expected then floatClose accuracy actual expected message
/// Expects `actual` to be greater than `expected` or to be within a
/// given `accuracy`.
let floatGreaterThanOrClose accuracy actual expected message =
    if actual<expected then floatClose accuracy actual expected message

/// Expect the passed float to be a number.
let isNotNaN f message =
  if Double.IsNaN f then failtestf "%s. Float was the NaN (not a number) value." message

/// Expect the passed float not to be positive infinity.
let isNotPositiveInfinity actual message =
  if Double.IsPositiveInfinity actual then failtestf "%s. Float was positive infinity." message

/// Expect the passed float not to be negative infinity.
let isNotNegativeInfinity actual message =
  if Double.IsNegativeInfinity actual then failtestf "%s. Float was negative infinity." message

/// Expect the passed float not to be infinity.
let isNotInfinity actual message =
  isNotNegativeInfinity actual message
  isNotPositiveInfinity actual message
  // passed via excluded middle

/// Expect the passed string not to be empty.
let isNotEmpty (actual : string) message =
  isNotNull actual message
  if String.IsNullOrWhiteSpace actual then Tests.failtestf "%s. Should not be empty." message

/// Expect the passed string is not whitespace
let isNotWhitespace (actual : string) message =
  isNotEmpty actual message
  let rec checkWhitespace index =
    if Char.IsWhiteSpace actual.[index] then
      if index < (actual.Length - 1) then checkWhitespace (index + 1)
      else failtestf "%s. Should not be whitespace." message
  checkWhitespace 0

/// Expect the passed sequence to be empty.
let isEmpty actual message =
  if not (Seq.isEmpty actual) then failtestf "%s. Should be empty." message

/// Expect the passed sequence not to be empty.
let isNonEmpty actual message =
  if Seq.isEmpty actual then failtestf "%s. Should not be empty." message

/// Expect that the counts of the found value occurrences in sequence equal the expected.
let hasCountOf (actual : _ seq) (expected : uint32) (selector : _ -> bool) message =
  let hits =
    actual |> Seq.fold (fun acc element -> if selector element then acc + 1u else acc) 0u
  if hits <> expected then failtestf "%s. Should be of count: %d, but was: %d" message expected hits

let private stringEquals actual expected message =
  match firstDiff actual expected with
  | _,None,None -> ()
  | i,Some a,Some e ->
    failtestf "%s. String does not match at position %i. Expected char: %A, but got %A.%s"
      message i e a (printVerses "expected" expected "  actual" actual)
  | i,None,Some e ->
    failtestf "%s. String actual was shorter than expected, at pos %i for expected item %A.%s"
      message i e (printVerses "expected" expected "  actual" actual)
  | i,Some a,None ->
    failtestf "%s. String actual was longer than expected, at pos %i found item %A.%s"
      message i a (printVerses "expected" expected "  actual" actual)

/// Expect the string `subject` to start with `prefix`. If it does not
/// then fail with `message` as an error message together with a description
/// of `subject` and `prefix`.
let stringStarts subject prefix message =
  match firstDiff subject prefix with
  | _,_,None -> ()
  | i,None,Some p ->
    failtestf
      "%s. Expected subject string to be longer or equal to prefix. Differs at position %i with char '%c'.%s"
      message i p (printVerses " prefix" prefix "subject" subject)
  | i,Some s,Some p ->
    failtestf
      "%s. Expected subject string to start with the prefix. Differs at position %i with subject '%c' and prefix '%c'.%s"
      message i s p (printVerses " prefix" prefix "subject" subject)

/// Expects the two values to equal each other.
let equal (actual : 'a) (expected : 'a) message =
  match box actual, box expected with
  | (:? string as a), (:? string as e) ->
    stringEquals a e message
  | (:? float as a), (:? float as e) ->
    if a <> e then
      failtestf "%s. Actual value was %f but had expected it to be %f." message a e
  | a, e ->
    if actual <> expected then
      if not (isNull' a) && FSharpType.IsRecord(a.GetType(), BindingFlags.Default) then
        let value (elem: obj) previous =
          (elem :?> PropertyInfo).GetValue(previous, null)
        let ai = (FSharpType.GetRecordFields (a.GetType(), BindingFlags.Public)).GetEnumerator()
        let ei = (FSharpType.GetRecordFields (e.GetType(), BindingFlags.Public)).GetEnumerator()
        let name() = (ai.Current :?> PropertyInfo).Name
        let mutable i = 0
        while ei.MoveNext() do
          if ai.MoveNext() then
            let currentA = value ai.Current a
            let currentE = value ei.Current e
            if currentA = currentE then ()
            else
              failtestf "%s.\nRecord does not match at position %i for field named `%s`. Expected field with value: %A, but got %A.%s"
                message (i + 1) (name()) currentE currentA (printVerses "expected" expected "  actual" actual)
          i <- i + 1
      else
        failtestf "%s.\n%s" message (printVerses "expected" expected "\nactual" actual)

/// Expects the two values not to equal each other.
let notEqual (actual : 'a) (expected : 'a) message =
  if expected = actual then
    failtestf "%s. Actual value was equal to %A but had expected them to be non-equal."
        message actual

/// Expects that actual matches pattern.
let isMatch actual pattern message =
  if Regex.Match(actual, pattern).Success then ()
  else failtestf "%s. Expected %s to match pattern: /%s/" message actual pattern

/// Expects that actual matches regex.
let isRegexMatch actual (regex : Regex) message =
  if regex.Match(actual).Success then ()
  else failtestf "%s. Expected %s to match regex: /%A/" message actual regex

/// Expects that matched groups (from a pattern match) match with matches operator
let isMatchGroups actual pattern (matchesOperator : GroupCollection -> bool) message =
  let groups = Regex.Match(actual, pattern).Groups
  if matchesOperator groups then ()
  else failtestf "%s. Expected %s to match pattern: /%s/ and apply to operator function: %A" message actual pattern matchesOperator

/// Expects that matched groups (from a regex match) match with matches operator
let isMatchRegexGroups actual (regex : Regex) (matchesOperator : GroupCollection -> bool) message =
  let groups = regex.Match(actual).Groups
  if matchesOperator groups then ()
  else failtestf "%s. Expected %s to match regex: /%A/ and apply to operator function: %A" message actual regex matchesOperator

/// Expects that actual not matches pattern.
let isNotMatch actual pattern message =
  if Regex.Match(actual, pattern).Success then Tests.failtestf "%s. Expected %s to match pattern: /%s/" message actual pattern

/// Expects that actual not matches regex.
let isNotRegexMatch actual (regex : Regex) message =
  if regex.Match(actual).Success then Tests.failtestf "%s. Expected %s to match regex: /%A/" message actual regex

/// Expects the value to be false.
let isFalse actual message =
  if not actual then ()
  else
    failtestf "%s. Actual value was true but had expected it to be false." message

/// Expects the value to be true.
let isTrue actual message =
  if actual then ()
  else
    failtestf "%s. Actual value was false but had expected it to be true." message

/// Expect that some element from `actual` satisfies the given `asserter`
let exists ( actual: 'a seq) asserter message =
  let exist =
    match actual with
    | null -> false
    | _ -> actual |> Seq.exists asserter
  if exist then ()
  else failtestf "%s. There isn't any element which satisfies given assertion %A." message asserter

let private allEqualTo actual asserter =
  actual
  |> Seq.indexed
  |> Seq.choose (fun (index, item) ->
    if not <| asserter item
    then Some (index, sprintf "%A" item)
    else None)
  |> List.ofSeq

let private formatAllEqualTo actual =
  actual
  |> Seq.map ( fun x -> sprintf "Element at index: %d which is equal to: %A" (fst x) (snd x))
  |> String.concat "\n"

/// Expect that all elements from `actual` satisfies the given `asserter`
let all (actual: 'a seq) asserter message =
  match actual with
  | null -> failtestf "%s. Sequence is empty" message
  | _ ->
    let checkResult = allEqualTo actual asserter
    if Seq.isEmpty checkResult then ()
    else failtestf "%s. Some elements don't satisfy `asserter`.\n%s" message (formatAllEqualTo checkResult)

/// Expect that all elements from `actual` are equal to `equalTo`
let allEqual (actual: 'a seq) equalTo message =
  match actual with
  | null -> failtestf "%s. Sequence is empty" message
  | _ ->
    let checkResult = allEqualTo actual ((=) equalTo)
    if Seq.isEmpty checkResult then ()
    else failtestf "%s. Some elements don't equal to `equalTo`: %A.\n%s" message equalTo (formatAllEqualTo checkResult)

/// Expects the `sequence` to contain the `element`.
let contains sequence element message =
  match sequence |> Seq.tryFind ((=) element) with
  | Some _ -> ()
  | None ->
    failtestf "%s. Sequence did not contain %A." message element

let private formatSet<'a> (concatBy) (formatResult) (whenEmpty) (ss : 'a seq) : string =
  if Seq.isEmpty ss then
    whenEmpty
  else
    (match box (Seq.item 0 ss) with
    | :? IComparable ->
      ss
      |> Seq.cast<IComparable>
      |> Seq.sort
      |> Seq.cast<'a>
      |> Seq.map (fun (a : 'a) -> a.ToString())
    | _ ->
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
                message =
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
              message (formatResult axs) (formatResult exs) (formatResult missing) (formatResult extra)
  |> failtest

let private except (elementsToCheck: Map<_,uint32>) (elementsToContain: Map<_,uint32>) (isExcept: bool) =
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
  |> Map.filter (fun _ found -> snd found <> 0ul)
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
                message =
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
              message (formatInput actual) (formatInput formatedExpected) missingElementsInfo extraElementsInfo
  |> failtest

let private printSeq xs =
    Seq.mapi (fun i x -> sprintf "  [%i] %A" i x) xs
    |> String.concat Environment.NewLine

/// Expects the `actual` sequence to equal the `expected` one.
let sequenceEqual actual expected message =
  let baseMsg() = printVerses "expected" (printSeq expected) "  actual" (printSeq actual)
  match firstDiff actual expected with
  | _,None,None -> ()
  | i,Some a, Some e ->
    failtestf "%s. Sequence does not match at position %i. Expected item: %A, but got %A.%s"
      message i e a (baseMsg())
  | i,None,Some e ->
    failtestf "%s. Sequence actual shorter than expected, at pos %i for expected item %A.%s"
      message i e (baseMsg())
  | i,Some a,None ->
    failtestf "%s. Sequence actual longer than expected, at pos %i found item %A.%s"
      message i a (baseMsg())

/// Expect the sequence `subject` to start with `prefix`. If it does not
/// then fail with `message` as an error message together with a description
/// of `subject` and `prefix`.
let sequenceStarts subject prefix message =
  match firstDiff subject prefix with
  | _,_,None -> ()
  | i,Some s,Some p ->
    failtestf "%s. Sequence does not match at position %i. Expected: %A, but got %A."
      message i p s
  | i,None,Some p ->
    failtestf "%s. Sequence actual shorter than expected, at pos %i for expected item %A."
      message i p

/// Expect the sequence `actual` to contains elements from sequence `expected` in the right order.
let sequenceContainsOrder (actual: seq<'t>) (expected: seq<'t>) msg =
  let el = Collections.Generic.Queue<'t> expected
  use ae = actual.GetEnumerator()
  let nl = Environment.NewLine
  let al = ResizeArray<'t>()
  let missingFail expected iter missing =
    failtestf "%s. Remainder of expected enumerable:%s%s%sWent through actual enumerable (%i items):%s%s%s" msg nl (printSeq expected) nl iter (printSeq missing) nl nl

  let rec check i =
    if el.Count = 0 then ()
    else
      if not (ae.MoveNext()) then missingFail el i al
      else
        al.Add ae.Current
        let expect = el.Peek()
        if expect = ae.Current then
          ignore (el.Dequeue())
          check (i + 1)
        else
          check (i + 1)

  check 0

/// Expect the sequence `subject` to be ascending. If it does not
/// then fail with `message` as an error message.
let isAscending (subject : _ seq) message =
  if not (subject |> Seq.windowed 2 |> Seq.forall (fun s -> s.[1] >= s.[0])) then
    failtestf "%s. Sequence is not ascending" message

/// Expect the sequence `subject` to be descending. If it does not
/// then fail with `message` as an error message.
let isDescending  (subject : _ seq) message =
  if not (subject |> Seq.windowed 2 |> Seq.forall (fun s -> s.[1] <= s.[0])) then
    failtestf "%s. Sequence is not descending" message

/// Expect the string `subject` to contain `substring` as part of itself.
/// If it does not, then fail with `message` and `subject` and `substring`
/// as part of the error message.
let stringContains (subject : string) (substring : string) message =
  if not (subject.Contains(substring)) then
    failtestf "%s. Expected subject string '%s' to contain substring '%s'."
                    message subject substring

/// Expect the string `subject` to end with `suffix`. If it does not
/// then fail with `message` as an error message together with a description
/// of `subject` and `suffix`.
let stringEnds (subject : string) (suffix : string) message =
  if not (subject.EndsWith suffix) then
    failtestf "%s. Expected subject string '%s' to end with '%s'."
                    message subject suffix

/// Expect the string `subject` to have length equals `length`. If it does not
/// then fail with `message` as an error message together with a description
/// of `subject` and `length`.
let stringHasLength (subject : string) (length : int) message =
  if subject.Length <> length then
    failtestf "%s. Expected subject string '%s' to have length '%d'."
                    message subject length

/// Expect length of a list/array/seq to be exactly length. Print out the whole seq
/// on failure so it's easier to debug.
let hasLength (seq: 'a seq) expectedLength message =
  let actualLength = Seq.length seq
  if actualLength <> expectedLength then
    failtestf "%s. Expected list to have length %d, but length was %d. Seq:\n%A"
      message expectedLength actualLength seq

/// Expect the streams to byte-wise equal.
let streamsEqual (s1 : IO.Stream) (s2 : IO.Stream) message =
  let buf = Array.zeroCreate<byte> 2
  let rec compare pos =
    match s1.Read(buf, 0, 1), s2.Read(buf, 1, 1) with
    | x, y when x <> y ->
      failtestf "%s. Not equal at pos %d" message pos
    | 0, _ ->
      ()
    | _ when buf.[0] <> buf.[1] ->
      failtestf "%s. Not equal at pos %d" message pos
    | _ ->
      compare (pos + 1)
  compare 0

/// Expects function `f1` is faster than `f2`. Measurer used to measure only a
/// subset of the functions. Statistical test to 99.99% confidence level.
let isFasterThanSub (f1:Performance.Measurer<_,_>->'a) (f2:Performance.Measurer<_,_>->'a) format =
  let toString (s:SampleStatistics) =
    sprintf "%.4f ± %.4f ms" s.mean s.meanStandardError

  match Performance.timeCompare f1 f2 with
  | Performance.ResultNotTheSame (r1, r2)->
    failtestNoStackf
      "%s. Expected function results to be the same (%A vs %A)." format r1 r2
  | Performance.MetricTooShort (s,p) ->
    failtestNoStackf
      "%s. Expected metric (%s) to be much longer than the machine resolution (%s)."
      format (toString s) (toString p)
  | Performance.MetricEqual (s1,s2) ->
    failtestNoStackf
      "%s. Expected f1 (%s) to be faster than f2 (%s) but are equal."
      format (toString s1) (toString s2)
  | Performance.MetricMoreThan (s1,s2) ->
    failtestNoStackf
      "%s. Expected f1 (%s) to be faster than f2 (%s) but is ~%.0f%% slower."
      format (toString s1) (toString s2) ((s1.mean/s2.mean-1.0)*100.0)
  | Performance.MetricLessThan (s1,s2) ->
    Impl.logger.log Info (
      eventX "{message}. f1 ({sample1}) is {percent} faster than f2 ({sample2})."
      >> setField "message" format
      >> setField "sample1" (toString s1)
      >> setField "percent" (sprintf "~%.0f%%" ((1.0-s1.mean/s2.mean)*100.0))
      >> setField "sample2" (toString s2))
    |> Async.StartImmediate

/// Expects function `f1` is faster than `f2`. Statistical test to 99.99%
/// confidence level.
let isFasterThan (f1:unit->'a) (f2:unit->'a) message =
  isFasterThanSub (fun measurer -> measurer f1 ())
                  (fun measurer -> measurer f2 ())
                  message
