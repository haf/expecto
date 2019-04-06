/// The logging namespace, which contains the logging abstraction for this
/// library. See https://github.com/logary/logary for details. This module is
/// completely stand-alone in that it has no external references and its adapter
/// in Logary has been well tested.
///
/// This file is licensed under the Apache 2.0 license without modifications.
/// This license applies to v3 of the Logary Facade. You can copy and paste this
/// code into your software, which freezes this license in place.
///
/// Original Source:
/// https://github.com/logary/logary/blob/996bdf92713f406b17c6cd7284e4d674f49e3ff6/src/Logary.Facade/Facade.fs
///
namespace Expecto.Logging

#nowarn "9"

open System
open System.Text

type ColourLevel =
  | Colour0
  | Colour8
  | Colour256

/// The log level denotes how 'important' the gauge or event message is.
[<CustomEquality; CustomComparison>]
type LogLevel =
  /// The log message is not that important; can be used for intricate debugging.
  | Verbose
  /// The log message is at a default level, debug level. Useful for shipping to
  /// infrastructure that further processes it, but not so useful for human
  /// inspection in its raw format, except during development.
  | Debug
  /// The log message is informational; e.g. the service started, stopped or
  /// some important business event occurred.
  | Info
  /// The log message is a warning; e.g. there was an unhandled exception or
  /// an even occurred which was unexpected. Sometimes human corrective action
  /// is needed.
  | Warn
  /// The log message is at an error level, meaning an unhandled exception
  /// occurred at a location where it is deemed important to keeping the service
  /// running. A human should take corrective action.
  | Error
  /// The log message denotes a fatal error which cannot be recovered from. The
  /// service should be shut down. Human corrective action is needed.
  | Fatal

  /// Converts the LogLevel to a string
  override x.ToString () =
    match x with
    | Verbose -> "verbose"
    | Debug   -> "debug"
    | Info    -> "info"
    | Warn    -> "warn"
    | Error   -> "error"
    | Fatal   -> "fatal"

  /// Converts the string passed to a Loglevel.
  static member ofString (str: string) =
    if str = null then invalidArg "str" "may not be null"
    match str.ToLowerInvariant() with
    | "verbose" -> Verbose
    | "debug"   -> Debug
    | "info"    -> Info
    | "warn"    -> Warn
    | "error"   -> Error
    | "fatal"   -> Fatal
    | _         -> Info

  /// Turn the LogLevel into an integer
  member x.toInt () =
    (function
    | Verbose -> 1
    | Debug   -> 2
    | Info    -> 3
    | Warn    -> 4
    | Error   -> 5
    | Fatal   -> 6) x

  /// Turn an integer into a LogLevel
  static member ofInt i =
    (function
    | 1 -> Verbose
    | 2 -> Debug
    | 3 -> Info
    | 4 -> Warn
    | 5 -> Error
    | 6 -> Fatal
    | i -> failwithf "LogLevel matching integer %i is not available" i) i

  interface IComparable<LogLevel> with
    member x.CompareTo other =
      compare (x.toInt()) (other.toInt())

  static member op_LessThan (a, b) =
    (a :> IComparable<LogLevel>).CompareTo(b) < 0
  static member op_LessThanOrEqual (a, b) =
    (a :> IComparable<LogLevel>).CompareTo(b) <= 0
  static member op_GreaterThan (a, b) =
    (a :> IComparable<LogLevel>).CompareTo(b) > 0
  static member op_GreaterThanOrEqual (a, b) =
    (a :> IComparable<LogLevel>).CompareTo(b) >= 0

  override x.GetHashCode () =
    x.toInt ()

  interface IComparable with
    member x.CompareTo other =
      match other with
      | null ->
        1
      | :? LogLevel as tother ->
        (x :> IComparable<LogLevel>).CompareTo tother
      | _ ->
        failwithf "invalid comparison %A to %A" x other

  interface IEquatable<LogLevel> with
    member x.Equals other =
      x.toInt() = other.toInt()

  override x.Equals other =
    (x :> IComparable).CompareTo other = 0

/// Represents a logged value; either a Gauge or an Event.
type PointValue =
  /// An event is what it sounds like; something occurred and needs to be
  /// logged. Its field is named 'template' because it should not be interpolated
  /// with values; instead these values should be put in the 'fields' field of
  /// the Message.
  | Event of template:string
  /// This is as value for a metric, with a unit attached. The unit can be
  /// something like Seconds or Hz.
  | Gauge of value:float * units:string

/// The # of nanoseconds after 1970-01-01 00:00:00.
type EpochNanoSeconds = int64

/// Helper functions for transforming DateTime to timestamps in unix epoch.
module DateTime =

  /// Get the Logary timestamp off the DateTime.
  let timestamp (dt: DateTime): EpochNanoSeconds =
    (dt.Ticks - DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc).Ticks)
    * 100L

  /// Get the DateTimeOffset ticks off from the EpochNanoSeconds.
  let ticksUTC (epoch: EpochNanoSeconds): int64 =
    epoch / 100L
    + DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc).Ticks

/// Helper functions for transforming DateTimeOffset to timestamps in unix epoch.
module DateTimeOffset =

  /// Get the Logary timestamp off the DateTimeOffset.
  let timestamp (dt: DateTimeOffset): EpochNanoSeconds =
    (dt.Ticks - DateTimeOffset(1970, 1, 1, 0, 0, 0, TimeSpan.Zero).Ticks)
    * 100L

  /// Get the DateTimeOffset ticks from EpochNanoSeconds
  let ticksUTC (epoch: EpochNanoSeconds): int64 =
    epoch / 100L
    + DateTimeOffset(1970, 1, 1, 0, 0, 0, TimeSpan.Zero).Ticks

/// This is record that is logged. It's capable of representing both metrics
/// (gauges) and events. See https://github.com/logary/logary for details.
type Message =
  { /// The 'path' or 'name' of this data point. Do not confuse template in
    /// (Event template) = message.value
    name: string[]
    /// The main value for this metric or event. Either a Gauge or an Event. (A
    /// discriminated union type)
    value: PointValue
    /// The structured-logging data.
    fields: Map<string, obj>
    /// When? nanoseconds since UNIX epoch.
    timestamp: EpochNanoSeconds
    /// How important? See the docs on the LogLevel type for details.
    level: LogLevel }

  /// Gets the ticks for UTC since 0001-01-01 00:00:00 for this message. You
  /// can pass this value into a DateTimeOffset c'tor
  member x.utcTicks =
    DateTimeOffset.ticksUTC x.timestamp

  /// If you're looking for how to transform the Message's fields, then use the
  /// module methods rather than instance methods, since you'll be creating new
  /// values rather than changing an existing value.
  member x.README =
    ()

/// The logger is the interface for calling code to use for logging. Its
/// different functions have different semantics - read the docs for each
/// method to choose the right one for your use-case.
type Logger =
  /// Gets the name of the logger instance.
  abstract member name: string[]
  /// Logs with the specified log level with backpressure via the logging
  /// library's buffers *and* ACK/flush to the underlying message targets.
  ///
  /// Calls to this function will block the caller only while executing the
  /// callback (if the level is active).
  ///
  /// The returned async value will yield when the message has been flushed to
  /// the underlying message targets.
  ///
  /// You need to start the (cold) async value for the logging to happen.
  ///
  /// You should not do blocking/heavy operations in the callback.
  abstract member logWithAck: LogLevel -> (LogLevel -> Message) -> Async<unit>

  /// Logs with the specified log level with backpressure via the logging
  /// library's buffers.
  ///
  /// Calls to this function will block the caller only while executing the
  /// callback (if the level is active).
  ///
  /// The returned async value will yield when the message has been added to
  /// the buffers of the logging library.
  ///
  /// You need to start the (cold) async value for the logging to happen.
  ///
  /// You should not do blocking/heavy operations in the callback.
  abstract member log: LogLevel -> (LogLevel -> Message) -> Async<unit>

/// Syntactic sugar on top of Logger for F# libraries.
[<AutoOpen>]
module LoggerEx =
  let private logWithTimeout (logger: Logger) level messageFactory =
    async {
      let! child = Async.StartChild (logger.log level messageFactory, 5000)
      try
        do! child
      with
      | :? TimeoutException ->
        Console.Error.WriteLine(
          "Logary (facade) message timed out. This means that you have an underperforming target. (Reevaluated) message name '{0}'.",
          String.concat "." (messageFactory level).name)
    }

  type Logger with
    member x.verbose (messageFactory: LogLevel -> Message): unit =
      logWithTimeout x Verbose messageFactory |> Async.Start

    /// Log with backpressure
    member x.verboseWithBP (messageFactory: LogLevel -> Message): Async<unit> =
      x.log Verbose messageFactory

    member x.debug (messageFactory: LogLevel -> Message): unit =
      logWithTimeout x Debug messageFactory |> Async.Start

    /// Log with backpressure
    member x.debugWithBP (messageFactory: LogLevel -> Message): Async<unit> =
      x.log Debug messageFactory

    member x.info (messageFactory: LogLevel -> Message): unit =
      logWithTimeout x Info messageFactory |> Async.Start

    /// Log with backpressure
    member x.infoWithBP (messageFactory: LogLevel -> Message): Async<unit> =
      x.log Info messageFactory

    member x.warn (messageFactory: LogLevel -> Message): unit =
      logWithTimeout x Warn messageFactory |> Async.Start

    /// Log with backpressure
    member x.warnWithBP (messageFactory: LogLevel -> Message): Async<unit> =
      x.log Warn messageFactory

    member x.error (messageFactory: LogLevel -> Message): unit =
      logWithTimeout x Error messageFactory |> Async.Start

    /// Log with backpressure
    member x.errorWithBP (messageFactory: LogLevel -> Message): Async<unit> =
      x.log Error messageFactory

    member x.fatal (messageFactory: LogLevel -> Message): unit =
      logWithTimeout x Fatal messageFactory |> Async.Start

    /// Log with backpressure
    member x.fatalWithBP (messageFactory: LogLevel -> Message): Async<unit> =
      x.log Fatal messageFactory

    /// Log a message, but don't synchronously wait for the message to be placed
    /// inside the logging library's buffers. Instead the message will be added
    /// to the logging library's buffers asynchronously (with respect to the
    /// caller) with a timeout of 5 seconds, and will then be dropped.
    ///
    /// This is the way we avoid the unbounded buffer problem.
    ///
    /// If you have dropped messages, they will be logged to STDERR. You should load-
    /// test your app to ensure that your targets can send at a rate high enough
    /// without dropping messages.
    ///
    /// It's recommended to have alerting on STDERR.
    member x.logSimple message: unit =
      logWithTimeout x message.level (fun _ -> message) |> Async.Start

type LoggingConfig =
  { timestamp: unit -> int64
    getLogger: string[] -> Logger
    consoleSemaphore: obj
  }
  static member create u2ts n2l sem = {
    timestamp = u2ts
    getLogger = n2l
    consoleSemaphore = sem
  }

module Literate =
  /// The output tokens, which can be potentially coloured.
  type LiterateToken =
    | Text | Subtext
    | Punctuation
    | LevelVerbose | LevelDebug | LevelInfo | LevelWarning | LevelError | LevelFatal
    | KeywordSymbol | NumericSymbol | StringSymbol | OtherSymbol | NameSymbol
    | MissingTemplateField

  type LiterateOptions =
    { formatProvider: IFormatProvider
      theme: LiterateToken -> ConsoleColor
      getLogLevelText: LogLevel -> string
      printTemplateFieldNames: bool }

    static member create ?formatProvider =
      // note: literate is meant for human consumption, and so the default
      // format provider of 'Current' is appropriate here. The reader expects
      // to see the dates, numbers, currency, etc formatted in the local culture
      { formatProvider = defaultArg formatProvider Globalization.CultureInfo.CurrentCulture
        getLogLevelText = function
                | Debug -> "DBG"
                | Error -> "ERR"
                | Fatal -> "FTL"
                | Info ->"INF"
                | Verbose -> "VRB"
                | Warn -> "WRN"
        theme = function
                | Text -> ConsoleColor.White
                | Subtext -> ConsoleColor.Gray
                | Punctuation -> ConsoleColor.DarkGray
                | LevelVerbose -> ConsoleColor.DarkGray
                | LevelDebug -> ConsoleColor.Gray
                | LevelInfo -> ConsoleColor.White
                | LevelWarning -> ConsoleColor.Yellow
                | LevelError -> ConsoleColor.Red
                | LevelFatal -> ConsoleColor.Red
                | KeywordSymbol -> ConsoleColor.Blue
                | NumericSymbol -> ConsoleColor.Magenta
                | StringSymbol -> ConsoleColor.Cyan
                | OtherSymbol -> ConsoleColor.Green
                | NameSymbol -> ConsoleColor.Gray
                | MissingTemplateField -> ConsoleColor.Red
        printTemplateFieldNames = false }

    static member createInvariant() =
      LiterateOptions.create Globalization.CultureInfo.InvariantCulture

/// Module that contains the 'known' keys of the Maps in the Message type's
/// fields/runtime data.
module Literals =

  /// What version of the Facade is this. This is a major version that allows the Facade
  /// adapter to choose how it handles the API.
  let FacadeVersion = 3u

  /// What language this Facade has. This controls things like naming standards.
  let FacadeLanguage = "F#"

  [<Literal>]
  let FieldExnKey = "exn"

  [<Literal>]
  let FieldErrorsKey = "errors"

type DVar<'a> = private { mutable cell: 'a; event: Event<'a>; mutable changed: bool }

module DVar =
  open System.Threading
  let create (x: 'x) = { cell = x; event = new Event<'x>(); changed = false }
  let get (xD: DVar<'x>) = xD.cell
  let wasChanged (xD: DVar<'x>) = xD.changed
  let put (x: 'x) (xD: DVar<'x>) =
    let prevX = Interlocked.Exchange (&xD.cell, x) // last writer wins
    xD.changed <- true // monotonically reaches true, hence thread safe
    xD.event.Trigger x
  let changes (xD: DVar<'x>) = xD.event.Publish
  let subs (xD: DVar<'x>) (x2u: 'x -> unit) = xD |> changes |> Event.add x2u
  let apply (a2bD: DVar<'a -> 'b>) (aD: DVar<'a>): DVar<'b> =
    let b = (get a2bD) (get aD)
    let bD = create b
    subs a2bD <| fun a2b -> let b = a2b (get aD) in put b bD
    subs aD <| fun a -> let f = get a2bD in let b = f a in put b bD
    bD
  let map (x2y: 'x -> 'y) (xD: DVar<'x>): DVar<'y> = let yD = create (x2y (get xD)) in subs xD (x2y >> fun x -> put x yD); yD
  let bindToRef (xR: 'x ref) (xD: DVar<'x>) = xR := get xD; subs xD (fun a -> xR := a)
  module Operators =
    let (<!>) = map
    let (<*>) = apply

module internal FsMtParser =
  open System.Text

  type Property(name: string, format: string) =
    static let emptyInstance = Property("", null)
    static member empty = emptyInstance
    member x.name = name
    member x.format = format
    member internal x.AppendPropertyString(sb: StringBuilder, ?replacementName) =
      sb.Append("{")
        .Append(defaultArg replacementName name)
        .Append(match x.format with null | "" -> "" | _ -> ":" + x.format)
        .Append("}")
    override x.ToString() = x.AppendPropertyString(StringBuilder()).ToString()

  module internal ParserBits =

    let inline isLetterOrDigit c = System.Char.IsLetterOrDigit c
    let inline isValidInPropName c = c = '_' || System.Char.IsLetterOrDigit c
    let inline isValidInFormat c = c <> '}' && (c = ' ' || isLetterOrDigit c || System.Char.IsPunctuation c)
    let inline isValidCharInPropTag c = c = ':' || isValidInPropName c || isValidInFormat c

    [<Struct>]
    type Range(startIndex: int, endIndex: int) =
      member inline x.start = startIndex
      member inline x.``end`` = endIndex
      member inline x.length = (endIndex - startIndex) + 1
      member inline x.getSubstring (s: string) = s.Substring(startIndex, x.length)
      member inline x.isEmpty = startIndex = -1 && endIndex = -1
      static member inline substring (s: string, startIndex, endIndex) = s.Substring(startIndex, (endIndex - startIndex) + 1)
      static member inline empty = Range(-1, -1)

    let inline tryGetFirstCharInRange predicate (s: string) (range: Range) =
      let rec go i =
        if i > range.``end`` then -1
        else if not (predicate s.[i]) then go (i+1) else i
      go range.start

    let inline tryGetFirstChar predicate (s: string) first =
      tryGetFirstCharInRange predicate s (Range(first, s.Length - 1))

    let inline hasAnyInRange predicate (s: string) (range: Range) =
      match tryGetFirstChar (predicate) s range.start with
      | -1 ->
        false
      | i ->
        i <= range.``end``

    let inline hasAny predicate (s: string) = hasAnyInRange predicate s (Range(0, s.Length - 1))
    let inline indexOfInRange s range c = tryGetFirstCharInRange ((=) c) s range

    let inline tryGetPropInRange (template: string) (within: Range): Property =
      // Attempts to validate and parse a property token within the specified range inside
      // the template string. If the property insides contains any invalid characters,
      // then the `Property.Empty' instance is returned (hence the name 'try')
      let nameRange, formatRange =
        match indexOfInRange template within ':' with
        | -1 ->
          within, Range.empty // no format
        | formatIndex ->
          Range(within.start, formatIndex-1), Range(formatIndex+1, within.``end``) // has format part
      let propertyName = nameRange.getSubstring template
      if propertyName = "" || (hasAny (not<<isValidInPropName) propertyName) then
        Property.empty
      elif (not formatRange.isEmpty) && (hasAnyInRange (not<<isValidInFormat) template formatRange) then
        Property.empty
      else
        let format = if formatRange.isEmpty then null else formatRange.getSubstring template
        Property(propertyName, format)

    let findNextNonPropText (startAt: int) (template: string) (foundText: string->unit): int =
      // Finds the next text token (starting from the 'startAt' index) and returns the next character
      // index within the template string. If the end of the template string is reached, or the start
      // of a property token is found (i.e. a single { character), then the 'consumed' text is passed
      // to the 'foundText' method, and index of the next character is returned.
      let mutable escapedBuilder = Unchecked.defaultof<StringBuilder> // don't create one until it's needed
      let inline append (ch: char) = if not (isNull escapedBuilder) then escapedBuilder.Append(ch) |> ignore
      let inline createStringBuilderAndPopulate i =
        if isNull escapedBuilder then
          escapedBuilder <- StringBuilder() // found escaped open-brace, take the slow path
          for chIndex = startAt to i-1 do append template.[chIndex] // append all existing chars
      let rec go i =
        if i >= template.Length then
          template.Length // bail out at the end of the string
        else
          let ch = template.[i]
          match ch with
          | '{' ->
            if (i+1) < template.Length && template.[i+1] = '{' then
              createStringBuilderAndPopulate i; append ch; go (i+2)
            else i // found an open brace (potentially a property), so bail out
          | '}' when (i+1) < template.Length && template.[i+1] = '}' ->
            createStringBuilderAndPopulate i; append ch; go (i+2)
          | _ ->
            append ch; go (i+1)

      let nextIndex = go startAt
      if (nextIndex > startAt) then // if we 'consumed' any characters, signal that we 'foundText'
        if isNull escapedBuilder then
          foundText (Range.substring(template, startAt, nextIndex - 1))
        else
          foundText (escapedBuilder.ToString())
      nextIndex

    let findPropOrText (start: int) (template: string)
                       (foundText: string -> unit)
                       (foundProp: Property -> unit): int =
      // Attempts to find the indices of the next property in the template
      // string (starting from the 'start' index). Once the start and end of
      // the property token is known, it will be further validated (by the
      // tryGetPropInRange method). If the range turns out to be invalid, it's
      // not a property token, and we return it as text instead. We also need
      // to handle some special case here: if the end of the string is reached,
      // without finding the close brace (we just signal 'foundText' in that case).
      let nextInvalidCharIndex =
        match tryGetFirstChar (not << isValidCharInPropTag) template (start+1) with
        | -1 ->
          template.Length
        | idx ->
          idx

      if nextInvalidCharIndex = template.Length || template.[nextInvalidCharIndex] <> '}' then
        foundText (Range.substring(template, start, (nextInvalidCharIndex - 1)))
        nextInvalidCharIndex
      else
        let nextIndex = nextInvalidCharIndex + 1
        let propInsidesRng = Range(start + 1, nextIndex - 2)
        match tryGetPropInRange template propInsidesRng with
        | prop when not (obj.ReferenceEquals(prop, Property.empty)) ->
          foundProp prop
        | _ ->
          foundText (Range.substring(template, start, (nextIndex - 1)))
        nextIndex

  /// Parses template strings such as "Hello, {PropertyWithFormat:##.##}"
  /// and calls the 'foundTextF' or 'foundPropF' functions as the text or
  /// property tokens are encountered.
  let parseParts (template: string) foundTextF foundPropF =
    let tlen = template.Length
    let rec go start =
      if start >= tlen then () else
      match ParserBits.findNextNonPropText start template foundTextF with
      | next when next <> start ->
        go next
      | _ ->
        go (ParserBits.findPropOrText start template foundTextF foundPropF)
    go 0

module internal MessageTemplates =
  type internal TemplateToken = TextToken of text:string | PropToken of name: string * format: string
  let internal parseTemplate template =
    let tokens = ResizeArray<TemplateToken>()
    let foundText (text: string) = tokens.Add (TextToken text)
    let foundProp (prop: FsMtParser.Property) = tokens.Add (PropToken (prop.name, prop.format))
    FsMtParser.parseParts template foundText foundProp
    tokens

/// Internal module for converting message parts into theme-able tokens.
module internal LiterateTokenisation =
  open System.Text
  open Literals
  open Literate

  /// A piece of text with an associated `LiterateToken`.
  type TokenisedPart = string * LiterateToken

  /// Converts some part(s) of a `Message` into text with an associated `LiterateToken`.
  type LiterateTokeniser = LiterateOptions -> Message -> TokenisedPart list

  /// Chooses the appropriate `LiterateToken` based on the value `Type`.
  let getTokenTypeForValue (value: obj) =
    match value with
    | :? bool -> KeywordSymbol
    | :? int16 | :? int32 | :? int64 | :? decimal | :? float | :? double -> NumericSymbol
    | :? string | :? char -> StringSymbol
    | _ -> OtherSymbol

  /// Converts a `PointValue` into a sequence literate tokens. The returned `Set<string>` contains
  /// the names of the properties that were found in the `Event` template.
  let tokenisePointValue (options: LiterateOptions) (fields: Map<string, obj>) = function
    | Event template ->
      let themedParts = ResizeArray<TokenisedPart>()
      let matchedFields = ResizeArray<string>()
      let foundText (text: string) = themedParts.Add (text, Text)
      let foundProp (prop: FsMtParser.Property) =
        match Map.tryFind prop.name fields with
        | Some propValue ->
          // render using string.Format, so the formatting is applied
          let stringFormatTemplate = prop.AppendPropertyString(StringBuilder(), "0").ToString()
          let fieldAsText = String.Format (options.formatProvider, stringFormatTemplate, [| propValue |])
          let valueTokenType = getTokenTypeForValue propValue
          if options.printTemplateFieldNames then
            themedParts.Add ("["+prop.name+"] ", Subtext)
          matchedFields.Add prop.name
          themedParts.Add (fieldAsText, valueTokenType)

        | None ->
          themedParts.Add (prop.ToString(), MissingTemplateField)

      FsMtParser.parseParts template foundText foundProp
      Set.ofSeq matchedFields, (themedParts :> TokenisedPart seq)

    | Gauge (value, units) ->
      Set.empty, ([| sprintf "%f" value, NumericSymbol
                     sprintf "%s" units, KeywordSymbol |] :> TokenisedPart seq)

  /// Converts a single exception into a sequence of literate tokens.
  let tokeniseExn (options: LiterateOptions) (ex: exn) =
    let stackFrameLinePrefix = "   at" // 3 spaces
    let monoStackFrameLinePrefix = "  at" // 2 spaces
    use exnLines = new System.IO.StringReader(ex.ToString())
    let rec go (lines: TokenisedPart list) =
      match exnLines.ReadLine() with
      | null ->
        List.rev lines // finished reading
      | line ->
        if line.StartsWith(stackFrameLinePrefix) || line.StartsWith(monoStackFrameLinePrefix) then
          // subtext
          go ((line, Subtext) :: (Environment.NewLine, Text) :: lines)
        else
          // regular text
          go ((line, Text) :: (Environment.NewLine, Text) :: lines)
    go []

  /// Converts all exceptions in a `Message` into a sequence of literate tokens.
  let tokeniseMessageExns (context: LiterateOptions) message =
    let exnExceptionParts =
      match message.fields.TryFind FieldExnKey with
      | Some (:? Exception as ex) ->
        tokeniseExn context ex
      | _ ->
        [] // there is no spoon
    let errorsExceptionParts =
      match message.fields.TryFind FieldErrorsKey with
      | Some (:? List<obj> as exnListAsObjList) ->
        exnListAsObjList |> List.collect (function
          | :? exn as ex ->
            tokeniseExn context ex
          | _ ->
            [])
      | _ ->
        []

    exnExceptionParts @ errorsExceptionParts

  let tokeniseLogLevel = function
    | Verbose -> LevelVerbose
    | Debug -> LevelDebug
    | Info -> LevelInfo
    | Warn -> LevelWarning
    | Error -> LevelError
    | Fatal -> LevelFatal

  /// Converts a `Message` into sequence of literate tokens.
  let tokeniseMessage (options: LiterateOptions) (message: Message): TokenisedPart list =
    let formatLocalTime (utcTicks: int64) =
      DateTimeOffset(utcTicks, TimeSpan.Zero).LocalDateTime.ToString("HH:mm:ss", options.formatProvider),
      Subtext

    let _, themedMessageParts =
      message.value |> tokenisePointValue options message.fields

    let themedExceptionParts = tokeniseMessageExns options message

    [ yield "[", Punctuation
      yield formatLocalTime message.utcTicks
      yield " ", Subtext
      yield options.getLogLevelText message.level, tokeniseLogLevel message.level
      yield "] ", Punctuation
      yield! themedMessageParts
      if not (isNull message.name) && message.name.Length > 0 then
        yield " ", Subtext
        yield "<", Punctuation
        yield String.concat "." message.name, Subtext
        yield ">", Punctuation
      yield! themedExceptionParts
    ]

/// Internal module for formatting text for printing to the console.
module internal Formatting =
  open Literals
  open Literate
  open LiterateTokenisation

  let formatValue (fields: Map<string, obj>) (pv: PointValue) =
    let matchedFields, themedParts =
      tokenisePointValue (LiterateOptions.createInvariant()) fields pv
    matchedFields, System.String.Concat(themedParts |> Seq.map fst)

  /// let the ISO8601 love flow
  let defaultFormatter (message: Message) =
    let app (x: obj) (sb: StringBuilder) =
      sb.Append x |> ignore

    let formatLevel (level: LogLevel) =
      "[" + Char.ToUpperInvariant(level.ToString().[0]).ToString() + "] "

    let formatInstant (utcTicks: int64) =
      (DateTimeOffset(utcTicks, TimeSpan.Zero).ToString("o")) + ": "

    let formatName (name: string[]) =
      " [" + String.concat "." name + "]"

    let formatExn (fields: Map<string, obj>) =
      match fields |> Map.tryFind FieldExnKey with
      | None ->
        String.Empty
      | Some ex ->
        " exn:\n" + ex.ToString()

    let formatFields (ignored: Set<string>) (fields: Map<string, obj>) =
      if not (Map.isEmpty fields) then
        fields
        |> Seq.filter (fun (KeyValue (k, _)) ->
          not (ignored |> Set.contains k))
        |> Seq.map (fun (KeyValue (k, v)) ->
          sprintf "\n - %s: %O" k v)
        |> String.concat ""
      else
        ""

    let matchedFields, valueString =
      formatValue message.fields message.value

    // [I] 2014-04-05T12:34:56Z: Hello World! [my.sample.app]
    formatLevel message.level +
    formatInstant message.utcTicks +
    valueString +
    formatName message.name +
    formatExn message.fields +
    formatFields matchedFields message.fields

/// Assists with controlling the output of the `LiterateConsoleTarget`.
module internal LiterateFormatting =
  open Literate
  open LiterateTokenisation
  open MessageTemplates

  type ColouredTextPart = string * ConsoleColor

  /// Writes string*ConsoleColor parts atomically (in a `lock`)
  let atomicallyWriteColouredTextToConsole sem (parts: ColouredTextPart list) =
    lock sem <| fun _ ->
      let originalColour = Console.ForegroundColor
      let mutable currentColour = originalColour
      parts |> List.iter (fun (text, colour) ->
        if currentColour <> colour then
          Console.ForegroundColor <- colour
          currentColour <- colour
        Console.Write(text)
      )
      if currentColour <> originalColour then
        Console.ForegroundColor <- originalColour

  [<AutoOpen>]
  module OutputTemplateTokenisers =
    open System.Collections.Generic

    let exceptionFieldNames = set [ Literals.FieldExnKey; Literals.FieldErrorsKey ]
    let tokeniseExtraField (options: LiterateOptions) (message: Message) (field: KeyValuePair<string, obj>) =
      seq {
        yield " - ", Punctuation
        yield field.Key, NameSymbol
        yield ": ", Punctuation
        yield System.String.Format(options.formatProvider, "{0}", field.Value), getTokenTypeForValue field.Value
      }

    let tokeniseExtraFields (options: LiterateOptions) (message: Message) (templateFieldNames: Set<string>) =
      let fieldsToExclude = Set.union templateFieldNames exceptionFieldNames
      let extraFields = message.fields |> Map.filter (fun key _ -> not (fieldsToExclude.Contains key))
      let mutable isFirst = true
      seq {
        for field in extraFields do
          if isFirst then isFirst <- false
          else yield Environment.NewLine, Text
          yield! tokeniseExtraField options message field
      }

    let tokeniseTimestamp format (options: LiterateOptions) (message: Message) =
      let localDateTimeOffset = DateTimeOffset(message.utcTicks, TimeSpan.Zero).ToLocalTime()
      let formattedTimestamp = localDateTimeOffset.ToString(format, options.formatProvider)
      seq { yield formattedTimestamp, Subtext }

    let tokeniseTimestampUtc format (options: LiterateOptions) (message: Message) =
      let utcDateTimeOffset = DateTimeOffset(message.utcTicks, TimeSpan.Zero)
      let formattedTimestamp = utcDateTimeOffset.ToString(format, options.formatProvider)
      seq { yield formattedTimestamp, Subtext }

    let tokeniseMissingField name format =
      seq {
        yield "{", Punctuation
        yield name, MissingTemplateField
        if not (String.IsNullOrEmpty format) then
          yield ":", Punctuation
          yield format, Subtext
        yield "}", Punctuation }

    let tokeniseLogLevel (options: LiterateOptions) (message: Message) =
      seq { yield options.getLogLevelText message.level, tokeniseLogLevel message.level }

    let tokeniseSource (options: LiterateOptions) (message: Message) =
      seq { yield (String.concat "." message.name), Subtext }

    let tokeniseNewline (options: LiterateOptions) (message: Message) =
      seq { yield Environment.NewLine, Text }

    let tokeniseTab (options: LiterateOptions) (message: Message) =
      seq { yield "\t", Text }

  /// Creates a `LiterateTokeniser` function which can be used by the `LiterateConsoleTarget`
  /// to customise how each log message is rendered. The default output template
  /// would be: `[{timestamp:HH:mm:ss} {level}] {message}{newline}{exceptions}`.
  /// Available template fields are: `timestamp`, `timestampUtc`, `level`, `source`,
  /// `newline`, `tab`, `message`, `properties`, `exceptions`. A special field named
  /// `newLineIfNext` will output a new line if the next field in the template will render
  /// anything (i.e. non-empty). Any misspelled or different property names will become a
  /// `LiterateToken.MissingTemplateField`.
  let tokeniserForOutputTemplate template: LiterateTokeniser =
    let tokens = parseTemplate template

    fun options message ->
      // render the message template first so we have the template-matched fields available
      let fieldsInMessageTemplate, messageParts =
        tokenisePointValue options message.fields message.value

      let tokeniseOutputTemplateField fieldName format = seq {
        match fieldName with
        | "timestamp" ->            yield! tokeniseTimestamp format options message
        | "timestampUtc" ->         yield! tokeniseTimestampUtc format options message
        | "level" ->                yield! tokeniseLogLevel options message
        | "source" ->               yield! tokeniseSource options message
        | "newline" ->              yield! tokeniseNewline options message
        | "tab" ->                  yield! tokeniseTab options message
        | "message" ->              yield! messageParts
        | "properties" ->           yield! tokeniseExtraFields options message fieldsInMessageTemplate
        | "exceptions" ->           yield! tokeniseMessageExns options message
        | _ ->                      yield! tokeniseMissingField fieldName format
      }

      seq {
        let lastTokenIndex = tokens.Count - 1
        let mutable nextPartsArray: TokenisedPart[] = null
        for index in [0..lastTokenIndex] do
          let token = tokens.[index]
          match token with
          | TextToken text -> yield text, LiterateToken.Punctuation
          | PropToken (name, format) ->
            if index <> lastTokenIndex && name = "newLineIfNext" then
              match tokens.[index + 1] with
              | PropToken (nextName, nextFormat) ->
                // Tokenise the next property now, to determine if it's 'empty'. To avoid doing
                // unnecessary work, we save these tokens ('nextPartsArray') so it can be
                // 'yield'ed on the next iteration.
                nextPartsArray <- tokeniseOutputTemplateField nextName nextFormat |> Seq.toArray
                if nextPartsArray.Length > 0 then
                  yield! tokeniseNewline options message
              | _ ->
                // It's questionable what to do here. It was an invalid output template,
                // because the {newLineIfNext} should only appear immediately prior to some other
                // valid output field. We could `failwith "invalid output template"`?
                ()
            else
              if not (isNull nextPartsArray) then
                yield! nextPartsArray
                nextPartsArray <- null
              else
                yield! tokeniseOutputTemplateField name format
      }
      |> Seq.toList

module internal ANSIOutputWriter =
  open System.IO
  open System.Runtime.InteropServices

  type private FuncTextWriter(encoding: Encoding, write: string -> unit) =
    inherit TextWriter()
    override __.Encoding = encoding
    override __.Write (s:string) = write s
    override __.WriteLine (s:string) = s + "\n" |> write
    override __.WriteLine() = write "\n"

  let mutable internal colours = None
  let internal setColourLevel c = if colours.IsNone then colours <- Some c

  let colourReset = "\u001b[0m"

  let private colour8BlackBG = function
    | ConsoleColor.Black -> "\u001b[30,1m"
    | ConsoleColor.DarkRed
    | ConsoleColor.Red -> "\u001b[31m"
    | ConsoleColor.DarkGreen
    | ConsoleColor.Green -> "\u001b[32m"
    | ConsoleColor.DarkYellow
    | ConsoleColor.Yellow -> "\u001b[33m"
    | ConsoleColor.DarkBlue
    | ConsoleColor.Blue -> "\u001b[34m"
    | ConsoleColor.DarkMagenta
    | ConsoleColor.Magenta -> "\u001b[35m"
    | ConsoleColor.DarkCyan
    | ConsoleColor.Cyan -> "\u001b[36m"
    | ConsoleColor.DarkGray
    | ConsoleColor.Gray
    | ConsoleColor.White -> "\u001b[37m"
    | _ -> ""

  let private colour8WhiteBG = function
    | ConsoleColor.Black -> "\u001b[30m"
    | ConsoleColor.DarkRed
    | ConsoleColor.Red -> "\u001b[31m"
    | ConsoleColor.DarkGreen
    | ConsoleColor.Green -> "\u001b[32m"
    | ConsoleColor.DarkYellow
    | ConsoleColor.Yellow -> "\u001b[33m"
    | ConsoleColor.DarkBlue
    | ConsoleColor.Blue -> "\u001b[34m"
    | ConsoleColor.DarkMagenta
    | ConsoleColor.Magenta -> "\u001b[35m"
    | ConsoleColor.DarkCyan
    | ConsoleColor.Cyan -> "\u001b[36m"
    | ConsoleColor.DarkGray
    | ConsoleColor.Gray
    | ConsoleColor.White -> "\u001b[30,1m"
    | _ -> ""

  let private colour256BlackBG =
    sprintf "\u001b[%sm" << function
    | ConsoleColor.Black -> "38;5;232"
    | ConsoleColor.DarkGray -> "38;5;234"
    | ConsoleColor.Gray -> "38;5;245"
    | ConsoleColor.DarkRed -> "38;5;52"
    | ConsoleColor.Red -> "38;5;1"
    | ConsoleColor.DarkGreen -> "38;5;28"
    | ConsoleColor.Green -> "38;5;40"
    | ConsoleColor.DarkYellow -> "38;5;220"
    | ConsoleColor.Yellow -> "38;5;11"
    | ConsoleColor.DarkBlue -> "38;5;18"
    | ConsoleColor.Blue -> "38;5;26"
    | ConsoleColor.DarkMagenta -> "38;5;55"
    | ConsoleColor.Magenta -> "38;5;165"
    | ConsoleColor.DarkCyan -> "38;5;31"
    | ConsoleColor.Cyan -> "38;5;39"
    | ConsoleColor.White -> "38;5;255"
    | _ -> ""

  let private colour256WhiteBG =
    sprintf "\u001b[%sm" << function
    | ConsoleColor.Black -> "38;5;255"
    | ConsoleColor.DarkGray -> "38;5;251"
    | ConsoleColor.Gray -> "38;5;245"
    | ConsoleColor.DarkRed -> "38;5;204"
    | ConsoleColor.Red -> "38;5;1"
    | ConsoleColor.DarkGreen -> "38;5;120"
    | ConsoleColor.Green -> "38;5;40"
    | ConsoleColor.DarkYellow -> "38;5;229"
    | ConsoleColor.Yellow -> "38;5;11"
    | ConsoleColor.DarkBlue -> "38;5;12"
    | ConsoleColor.Blue -> "38;5;26"
    | ConsoleColor.DarkMagenta -> "38;5;219"
    | ConsoleColor.Magenta -> "38;5;165"
    | ConsoleColor.DarkCyan -> "38;5;159"
    | ConsoleColor.Cyan -> "38;5;39"
    | ConsoleColor.White -> "38;5;232"
    | _ -> ""

  let private isBlackBG = Console.BackgroundColor = ConsoleColor.Black
                          || int Console.BackgroundColor = -1

  let colourText colourLevel colour =
    match colourLevel with
    | Colour0 -> String.Empty
    | Colour8 -> if isBlackBG then colour8BlackBG colour else colour8WhiteBG colour
    | Colour256 -> if isBlackBG then colour256BlackBG colour else colour256WhiteBG colour

  let private foregroundColor = Console.ForegroundColor

  module private WindowsConsole =
    open Microsoft.FSharp.NativeInterop

    [<DllImport("Kernel32")>]
    extern void* private GetStdHandle(int _nStdHandle)

    [<DllImport("Kernel32")>]
    extern bool private GetConsoleMode(void* _hConsoleHandle, int* _lpMode)

    [<DllImport("Kernel32")>]
    extern bool private SetConsoleMode(void* _hConsoleHandle, int _lpMode)

    /// https://superuser.com/questions/413073/windows-console-with-ansi-colors-handling
    let enableVTMode () =
#if NETSTANDARD2_0
      if not (RuntimeInformation.IsOSPlatform OSPlatform.Windows) then () else
#else
      if not (Environment.OSVersion.Platform = PlatformID.Win32NT) then () else
#endif
      let INVALID_HANDLE_VALUE = nativeint -1
      let STD_OUTPUT_HANDLE = -11
      let ENABLE_VIRTUAL_TERMINAL_PROCESSING = 0x0004
      let handle = GetStdHandle(STD_OUTPUT_HANDLE)
      if handle <> INVALID_HANDLE_VALUE then
        let mode = NativePtr.stackalloc<int> 1
        if GetConsoleMode(handle, mode) then
          let value = NativePtr.read mode
          let value = value ||| ENABLE_VIRTUAL_TERMINAL_PROCESSING
          SetConsoleMode(handle, value) |> ignore

  /// Lifecycle: (new T() -> t.init() -> t._ {0,*} -> (t :> IDisposable).Dispose()) {1,*}
  [<Sealed>]
  type T(origStdOut: TextWriter, origStdErr: TextWriter, sem: obj) =
    // Invariants:
    //  - For every non-private method that touches the origStdOut or origStdErr,
    //    a lock on `sem` must be held.
    //  - For every change to `buffer`, a lock on `buffer` must be held.
    //  - Call order of locks must always be; 1. `lock sem ..`, 2. `lock buffer ..`,
    //    or we may deadlock.
    //  - The call order described in the docs of this type must be followed.
    //  - No `..Inner` function declared below may take a lock on `sem`.
    //  - All `..Inner` functions below must assert `inited` is valid.
    //  - We must not throw if history init-dispose-(init | print) happens; reinitialising is allowed
    let mutable incompleteTextOutput: (string * ConsoleColor) list = []
    let mutable inited = false
    let buffer = StringBuilder()
    let flushStart = new Event<unit>()
    let flushEnd = new Event<unit>()
    
    let flushInner () =
      if inited then
        lock buffer <| fun _ ->
          flushStart.Trigger ()
          buffer.ToString() |> origStdOut.Write
          buffer.Clear() |> ignore
          flushEnd.Trigger ()

    // Change parts to Array since we have all items available when calling
    let rec prettyPrintInner (autoFlush, fromSysConsole) parts =
      ignore (tryInitInner ())

      lock buffer <| fun _ ->
        let hasEndLine =
          parts
          |> Seq.map fst
          |> Seq.where (String.IsNullOrEmpty >> not)
          |> Seq.tryLast
          |> Option.bind Seq.tryLast
          |> fun oc -> oc = Some '\n'

        if fromSysConsole && not hasEndLine then
          incompleteTextOutput <- incompleteTextOutput @ parts
        else
          let parts =
            if List.isEmpty incompleteTextOutput then parts
            else
              let parts = incompleteTextOutput @ parts
              incompleteTextOutput <- []
              parts

          let mutable currentColour = foregroundColor
          parts |> List.iter (fun (text, colour) ->
            if currentColour <> colour then
              colourText colours.Value colour |> buffer.Append |> ignore
              currentColour <- colour
            buffer.Append text |> ignore
          )
          buffer.Append colourReset |> ignore

          if autoFlush then flushInner ()

    and initInner () =
      if inited then invalidOp "Cannot init ANSIOutputWriter twice"
      inited <- true

      WindowsConsole.enableVTMode()
      origStdOut.Flush()

      let encoding = stdout.Encoding
      let std s = prettyPrintInner (true, true) [s, foregroundColor]
      Console.SetOut (new FuncTextWriter(encoding, std))

      let errorEncoding = origStdErr.Encoding
      let errorToOutput s = prettyPrintInner (true, true) [s, ConsoleColor.Red]
      Console.SetError (new FuncTextWriter(errorEncoding, errorToOutput))

    and tryInitInner () =
      if inited then false else
      initInner ()
      true

    let disposeInner () =
      // TODO: hunt down all usages of ANSIConsoleLogger and ensure they only init-use-dispose in that order
      if not inited then () else // invalidOp "Cannot Dispose unless inited"
      flushInner ()
      Console.SetOut origStdOut
      Console.SetError origStdErr
      inited <- false

    /// HERE BE DRAGONS
    ///
    /// Unbuffered, unlocked write and flush to the original stdout file descriptor. Only use this when you are sure that
    /// you want to be manipulating the state of the output buffer/console directly. Calls to this function does not
    /// trigger the FlushStart/FlushEnd events.
    ///
    /// This function is internal, because it needs to be guarded by a Monitor object; in the case of Expecto, this
    /// guard is in Progress.fs, an is the lock on the ref cell `isRunning`.
    member internal __.writeAndFlushRaw (value: string) =
      origStdOut.Write value
      origStdOut.Flush()

    // Invert control flow, from calling INTO the ProgressIndicator, to having the ProgressIndicator subscribe to the life
    // cycle events of the ANSIOutputWriter.

    /// This event is triggered when the ANSIOutputWriter is about to flush its internal buffer of characters to print to
    /// STDOUT. Events are synchronously dispatched on the caller thread.
    [<CLIEvent>]
    member internal __.FlushStart = flushStart.Publish

    /// This event is triggered when the ANSIOutputWriter has finished flusing its internal buffer of characters to to
    /// STDOUT. Events are synchronously dispatched on the caller thread.
    [<CLIEvent>]
    member internal __.FlushEnd = flushEnd.Publish

    /// Flushes the built-up buffer and clears it. Calling this function will trigger FlushStart and FlushEnd, in that
    /// order.
    member x.flush() =
      lock sem flushInner

    member internal __.prettyPrint autoFlush (parts : (string * ConsoleColor) list) : unit =
      lock sem (fun () -> prettyPrintInner (autoFlush, false) parts)

    /// During the time between calls x.init() -> IDisposable.Dispose(), there must be no other calls to `init`.
    ///
    /// This installs interceptors for both STDOUT and STDERR, having them go through this instance; `
    member __.init() =
      lock sem initInner

    member __.tryInit() =
      // don't take a lock unless we need to (test-lock-test pattern):
      if inited then false else lock sem tryInitInner

    interface IDisposable with
      /// Must correspond to a previous init call.
      member __.Dispose() =
        lock sem disposeInner

  let private instance: T option ref = ref None

  let create (sem: obj) : T =
    lock instance <| fun () ->
    !instance |> Option.iter (fun i -> (i :> IDisposable).Dispose())
    let x = new T(stdout, stderr, sem)
    instance := Some x
    x

  let internal getInstance sem =
    !instance |> Option.defaultWith (fun () -> create sem)

  let prettyPrint autoFlush sem =
    let i = getInstance sem
    i.prettyPrint autoFlush

  let flush () = !instance |> Option.iter (fun i -> i.flush())
  let close () = !instance |> Option.iter (fun i -> (i :> IDisposable).Dispose())
  let writeAndFlushRaw (value: string) = !instance |> Option.iter (fun i -> i.writeAndFlushRaw value)

/// Logs a line in a format that is great for human consumption,
/// using console colours to enhance readability.
/// Sample: [10:30:49 INF] User "AdamC" began the "checkout" process with 100 cart items
type LiterateConsoleTarget(name, minLevel, ?options, ?literateTokeniser, ?outputWriter, ?consoleSemaphore) =
  let sem          = defaultArg consoleSemaphore (obj())
  let options      = defaultArg options (Literate.LiterateOptions.create())
  let tokenise     = defaultArg literateTokeniser LiterateTokenisation.tokeniseMessage
  let colourWriter = outputWriter |> Option.defaultWith (fun () ->
    ANSIOutputWriter.prettyPrint (minLevel <= Debug) sem
  )

  /// Converts the message to tokens, apply the theme, then output them using the `colourWriter`.
  let writeColourisedThenNewLine message =
    [ yield! tokenise options message
      yield Environment.NewLine, Literate.Text ]
    |> List.map (fun (s, t) -> s, options.theme(t))
    |> colourWriter

  /// Creates the target with a custom output template.
  /// For example, the default output template would be:
  /// `[{timestamp:HH:mm:ss} {level}] {message} <{source}>{newLineIfNext}{exceptions}`.
  /// Available template fields are: `timestamp`, `timestampUtc`, `level`, `source`,
  /// `newline`, `tab`, `message`, `properties`, `exceptions`.
  /// A special field named `newLineIfNext` will output a new line if the next field renders
  /// anything (i.e. non-empty). Any other property names will become a
  /// `LiterateToken.MissingTemplateField`.
  new (name, minLevel, outputTemplate, ?options, ?outputWriter, ?consoleSemaphore) =
    let tokeniser = LiterateFormatting.tokeniserForOutputTemplate outputTemplate
    new LiterateConsoleTarget(name, minLevel,
          ?options=options, literateTokeniser=tokeniser, ?outputWriter=outputWriter, ?consoleSemaphore=consoleSemaphore)

  interface Logger with
    member __.name = name
    member __.logWithAck level msgFactory =
      if level >= minLevel then
        async { do writeColourisedThenNewLine (msgFactory level) }
      else
        async.Return ()
    member __.log level msgFactory =
      if level >= minLevel then
        async { do writeColourisedThenNewLine (msgFactory level) }
      else
        async.Return ()

type TextWriterTarget(name, minLevel, writer: IO.TextWriter, ?formatter) =
  let formatter = defaultArg formatter Formatting.defaultFormatter
  let log msg = writer.WriteLine(formatter msg)

  interface Logger with
    member __.name = name
    member __.log level messageFactory =
      if level >= minLevel then
        async { do log (messageFactory level) }
      else
        async.Return ()

    member __.logWithAck level messageFactory =
      if level >= minLevel then
        async { log (messageFactory level) }
      else
        async.Return ()

type OutputWindowTarget(name, minLevel, ?formatter) =
  let formatter = defaultArg formatter Formatting.defaultFormatter
  let log msg = System.Diagnostics.Debug.WriteLine(formatter msg)

  interface Logger with
    member __.name = name
    member __.log level messageFactory =
      if level >= minLevel then
        async { do log (messageFactory level) }
      else
        async.Return ()

    member __.logWithAck level messageFactory =
      if level >= minLevel then
        async { do log (messageFactory level) }
      else
        async.Return ()

/// A logger to use for combining a number of other loggers
type CombiningTarget(name, otherLoggers: Logger list) =
  interface Logger with
    member __.name = name
    member __.logWithAck level messageFactory =
      otherLoggers
      |> List.map (fun l -> l.logWithAck level messageFactory)
      |> Async.Parallel
      |> Async.Ignore // Async<unit>

    member __.log level messageFactory =
      otherLoggers
      |> List.map (fun l -> l.log level messageFactory)
      |> Async.Parallel
      |> Async.Ignore // Async<unit>

module Global =
  open DVar.Operators

  /// This is the global semaphore for writing to the console output. Ensure
  /// that the same semaphore is used across libraries by using the Logary
  /// Facade Adapter in the final composing app/service.
  let private semD = DVar.create (obj ())
  let private getTimestampD = DVar.create (fun () -> DateTimeOffset.timestamp DateTimeOffset.UtcNow)
  let private minLevelD = DVar.create Info
  let private getLoggerInnerD =
    DVar.create (fun level name ->
      LiterateConsoleTarget(name, level) :> Logger
    )
  let private getLoggerD : DVar<string[]->Logger> =
    DVar.apply getLoggerInnerD minLevelD

  let private cfgD : DVar<LoggingConfig> = LoggingConfig.create <!> getTimestampD <*> getLoggerD <*> semD

  let defaultConfig = DVar.get cfgD

  let private config =
    let cfgR = ref defaultConfig in cfgD |> DVar.bindToRef cfgR
    cfgR

  type internal Flyweight(name: string[]) =
    let loggerD = getLoggerD |> DVar.map (fun factory -> factory name)
    let ensureName (m: Message) = if Array.isEmpty m.name then { m with name = name } else m
    interface Logger with
      member __.name = name
      member __.log level msgFactory =
        let logger = DVar.get loggerD in logger.log level (msgFactory >> ensureName)
      member __.logWithAck level msgFactory =
        let logger = DVar.get loggerD in logger.logWithAck level (msgFactory >> ensureName)

  let internal getStaticLogger (name: string[]) = Flyweight name :> Logger

  /// Gets the current timestamp.
  let timestamp(): EpochNanoSeconds = let get = DVar.get getTimestampD in get ()

  /// Returns the synchronisation object to use when printing to the console.
  let internal semaphore () = DVar.get semD

  /// Run the passed function under the console semaphore lock.
  let internal lockSem fn = lock (semaphore ()) fn

  /// Call from the initialisation of your library. Initialises the Logary.Facade globally/per process.
  let initialise (cfg: LoggingConfig) =
    semD |> DVar.put cfg.consoleSemaphore
    getTimestampD |> DVar.put cfg.timestamp
    getLoggerInnerD |> DVar.put (fun _ name -> cfg.getLogger name)

  let initialiseIfDefault cfg = if not (DVar.wasChanged cfgD) then initialise cfg

/// "Shortcut" for creating targets; useful at the top-level configuration point of
/// your library.
module Targets =
  /// Create a new target. Prefer `Log.create` in your own libraries, or let the
  /// composing app replace your target instance through your configuration.
  ///
  /// Will log to console (colourised) by default, and also to the output window
  /// in your IDE if you specify a level below Info.
  let create level name =
    if level >= Info then
      LiterateConsoleTarget(name, level, consoleSemaphore = Global.semaphore()) :> Logger
    else
      CombiningTarget(
        name,
        [ LiterateConsoleTarget(name, level, consoleSemaphore = Global.semaphore())
          OutputWindowTarget(name, level) ])
      :> Logger

/// Module for acquiring static loggers (when you don't want or can't)
/// pass loggers as values.
module Log =

  /// Create an hierarchically named logger
  let createHiera (name: string[]) =
    if name = null then invalidArg "name" "name is null"
    if name.Length = 0 then invalidArg "name" "must have >0 segments"
    Global.getStaticLogger name

  /// Create a named logger. Full stop (.) acts as segment delimiter in the
  /// hierachy of namespaces and loggers.
  let create (name: string) =
    if name = null then invalidArg "name" "name is null"
    createHiera (name.Split([|'.'|], StringSplitOptions.RemoveEmptyEntries))

/// The Message module contains functions that can help callers compose messages. This
/// module is especially helpful to open to make calls into Logary's facade small.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Message =  
  open Literals

  /// Create a new event log message.
  let event level template =
    { name      = [||]
      value     = Event template
      fields    = Map.empty
      timestamp = Global.timestamp ()
      level     = level }

  /// Create a new event log message – like `event` but with parameters flipped.
  /// Useful to use with `Logger.log` with point-free style, to reduce the
  /// noise. E.g. `logger.logVerbose (eventX "Returned {code}" >> setField "code" 24)`
  let eventX template level =
    event level template

  /// Create a new instantaneous value in a log message.
  let gauge value units =
    { name      = [||]
      value     = Gauge (value, units)
      fields    = Map.empty
      timestamp = Global.timestamp ()
      level     = Debug }

  /// Sets the name/path of the log message.
  let setName (name: string[]) (x: Message) =
    { x with name = name }

  /// Sets the final portion o fthe name of the Message.
  let setNameEnding (ending: string) (x: Message) =
    if String.IsNullOrWhiteSpace ending then x else
    let segs = ResizeArray<_>(x.name)
    segs.Add ending
    { x with name = segs.ToArray() }

  /// Sets the name as a single string; if this string contains dots, the string
  /// will be split on these dots.
  let setSingleName (name: string) (x: Message) =
    if name = null then invalidArg "name" "may not be null"

    let name' =
      name.Split([|'.'|], StringSplitOptions.RemoveEmptyEntries)

    x |> setName name'

  /// Sets the value of the field on the log message.
  let setField (key: string) (value: obj) (x: Message) =
    { x with fields = x.fields |> Map.add key value }

  /// Alias to `setField`
  let setFieldValue = setField

  /// Sets the timestamp on the log message.
  let setTimestamp (ts: EpochNanoSeconds) (x: Message) =
    { x with timestamp = ts }

  /// Sets the level on the log message.
  let setLevel (level: LogLevel) (x: Message) =
    { x with level = level }

  /// Adds an exception to the Message, to the 'errors' field, inside a list.
  let addExn ex (x: Message) =
    let fields' =
      match Map.tryFind FieldErrorsKey x.fields with
      | None ->
        x.fields |> Map.add FieldErrorsKey (box [ box ex ])

      | Some errors ->
        let arr: obj list = unbox errors
        x.fields |> Map.add FieldErrorsKey (box (box ex :: arr))

    { x with fields = fields' }