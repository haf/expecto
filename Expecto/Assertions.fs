namespace Expecto

open System

type Expect =

    static member NotEqual(msg, negative_case, actual) =
        if negative_case = actual
            then failtestf "%s\nExpected: %A\nnot to equal Actual: %A" msg negative_case actual

    static member Equal(msg, expected, actual) =
        if expected <> actual
            then failtestf "%s\nExpected: %A\nActual: %A" msg expected actual

    static member None(msg, value) =
        match value with
        | Some x -> failtestf "%s\nExpected None, Actual: Some (%A)" msg x
        | _ -> ()

    static member NotNull<'a when 'a : null>(msg, actual: 'a) =
        match box actual with
        | null -> failtestf "%s\nShould not have been null" msg
        | _ -> ()

    static member Raise(msg, ex: Type, f) =
        try
            f()
            failtestf "%s\nExpected exception '%s' but no exception was raised" msg ex.FullName
        with e ->
            if e.GetType() <> ex
                then failtestf "%s\nExpected exception '%s' but raised:\n%A" msg ex.FullName e

    static member StringContains(msg, expectedSubString, actual: string) =
        if not (actual.Contains expectedSubString)
            then failtestf "%s\nExpected string containing: %s\nActual: %s" msg expectedSubString actual
        
