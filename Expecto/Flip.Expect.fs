module Expecto.Flip.Expect

/// Expects f to throw an exception.
let inline throws message f = Expecto.Expect.throws f message

/// Expects f to throw, and calls `cont` with its exception.
let inline throwsC cont f = Expecto.Expect.throwsC f cont

/// Expects the passed function to throw `'texn`.
let inline throwsT<'texn> message f = Expecto.Expect.throwsT f message

/// Expects the value to be a None value.
let inline isNone message x = Expecto.Expect.isNone x message

/// Expects the value to be a Some _ value.
let inline isSome message x = Expecto.Expect.isSome x message

/// Expects the value to be a Choice1Of2 value.
let inline isChoice1Of2 message x = Expecto.Expect.isChoice1Of2 x message

/// Expects the value to be a Choice2Of2 value.
let inline isChoice2Of2 message x = Expecto.Expect.isChoice2Of2 x message

/// Expects the value not to be null.
let inline isNotNull message x = Expecto.Expect.isNotNull x message

/// Expects the value to be null.
let inline isNull message x = Expecto.Expect.isNull x message

/// Expects `a` to be less than `b`.
let inline isLessThan message (a, b) = Expecto.Expect.isLessThan a b message

/// Expects `a` <= `b`.
let inline isLessThanOrEqual message (a, b) = Expecto.Expect.isLessThanOrEqual a b message

/// Expects `a` > `b`.
let inline isGreaterThan message (a, b) = Expecto.Expect.isGreaterThan a b message

/// Expects `a` >= `b`.
let inline isGreaterThanOrEqual message (a, b) = Expecto.Expect.isGreaterThanOrEqual a b message

/// Expects `actual` and `expected` (that are both floats) to be within a
/// given `accuracy`.
let inline floatClose message accuracy expected actual = Expecto.Expect.floatClose accuracy actual expected message

/// Expect the passed float to be a number.
let inline isNotNaN message f = Expecto.Expect.isNotNaN f message

/// Expect the passed float not to be positive infinity.
let inline isNotPositiveInfinity message f = Expecto.Expect.isNotPositiveInfinity f message

/// Expect the passed float not to be negative infinity.
let inline isNotNegativeInfinity message f = Expecto.Expect.isNotNegativeInfinity f message

/// Expect the passed float not to be infinity.
let inline isNotInfinity message f = Expecto.Expect.isNotInfinity f message

/// Expect the passed string not to be empty.
let inline isNotEmpty message actual = Expecto.Expect.isNotEmpty actual message

/// Expect the passed string is not whitespace
let inline isNotWhitespace message actual = Expecto.Expect.isNotWhitespace actual message

/// Expects the two values to equal each other.
let inline equal message expected actual = Expecto.Expect.equal actual expected message

/// Expects the two values not to equal each other.
let inline notEqual message expected actual = Expecto.Expect.notEqual actual expected message

/// Expects the value to be false.
let inline isFalse message actual = Expecto.Expect.isFalse actual message

/// Expects the value to be true.
let inline isTrue message actual = Expecto.Expect.isTrue actual message

/// Expects the `sequence` to contain the `element`.
let inline contains message element sequence = Expecto.Expect.contains sequence element message

/// Expects the `actual` sequence to contain all elements from `expected`
/// it doesn't take into account number of occurances of characters
/// sequence (not taking into account an order of elements). Calling this
/// function will enumerate both sequences; they have to be finite.
let inline containsAll message expected actual = Expecto.Expect.containsAll actual expected message

/// Expects the `actual` sequence to contain all elements from `expected` map,
/// first element in every tuple from `expected` map means item which should be
/// presented in `actual` sequence, the second element means an expected number of occurrences
/// of this item in sequence.
/// Function is not taking into account an order of elements.
/// Calling this function will enumerate both sequences; they have to be finite.
let inline distribution message expected actual = Expecto.Expect.distribution actual expected message

/// Expects the `actual` sequence to equal the `expected` one.
let inline sequenceEqual message expected actual = Expecto.Expect.sequenceEqual actual expected message

/// Expect the sequence `subject` to start with `prefix`. If it does not
/// then fail with `message` as an error message together with a description
/// of `subject` and `prefix`.
let inline sequenceStarts message prefix subject = Expecto.Expect.sequenceStarts subject prefix message

/// Expect the sequence `subject` to be ascending. If it does not
/// then fail with `message` as an error message.
let inline isAscending message subject = Expecto.Expect.isAscending subject message

/// Expect the sequence `subject` to be descending. If it does not
/// then fail with `message` as an error message.
let inline isDescending message subject = Expecto.Expect.isDescending subject message

/// Expect the string `subject` to contain `substring` as part of itself.
/// If it does not, then fail with `format` and `subject` and `substring`
/// as part of the error message.
let inline stringContains message substring subject = Expecto.Expect.stringContains subject substring message

/// Expect the string `subject` to start with `prefix`. If it does not
/// then fail with `message` as an error message together with a description
/// of `subject` and `prefix`.
let inline stringStarts message prefix subject = Expecto.Expect.stringStarts subject prefix message

/// Expect the string `subject` to end with `suffix`. If it does not
/// then fail with `message` as an error message together with a description
/// of `subject` and `suffix`.
let inline stringEnds message suffix subject = Expecto.Expect.stringEnds subject suffix message

/// Expect the string `subject` to have length equals `length`. If it does not
/// then fail with `format` as an error message together with a description
/// of `subject` and `length`.
let inline stringHasLength message length subject = Expecto.Expect.stringHasLength subject length message

/// Expect the streams to byte-wise equal.
let inline streamsEqual message s2 s1 = Expecto.Expect.streamsEqual s1 s2 message

/// Expects function `f1` is faster than `f2`. Measurer used to measure only a
/// subset of the functions. Statistical test to 99.99% confidence level.
let inline isFasterThanSub message f2 f1 = Expecto.Expect.isFasterThanSub f1 f2 message

/// Expects function `f1` is faster than `f2`. Statistical test to 99.99%
/// confidence level.
let inline isFasterThan message f2 f1 = Expecto.Expect.isFasterThan f1 f2 message