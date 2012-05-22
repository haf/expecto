namespace Fuchu

module XunitHelpers =
    open System

    let create (t: Type) =
        try
            Activator.CreateInstance t
        with e -> raise (Exception(sprintf "Couldn't instantiate test type %s" t.FullName, e))