module SimpleFunctionsCLI.ValueConversion

open System

/// Convert string value to given type;
/// supported types are: string, int, uint32.
let convertToType (matchType: Type) (value: string) =
    match matchType with
    | s when s = typeof<string> -> Some(value |> box)
    | i when i = typeof<int> -> Some(int value |> box)
    | u when u = typeof<uint32> -> Some(uint32 value |> box)
    | _ -> None

/// Convert string value to generic type 'T;
/// supported types are: string, int, uint32.
/// Type 'T should implement IConvertible
let convert<'T when 'T :> IConvertible> (value: string) =
    match typeof<'T> with
    | s when s = typeof<string> -> Some(value |> box)
    | i when i = typeof<int> -> Some(int value |> box)
    | u when u = typeof<uint32> -> Some(uint32 value |> box)
    | _ -> None

/// Converts string with values to array of given type.
let convertToArrayOfType (arrElemType: Type) (valuesString: string) (separator: string) =
    valuesString.Split separator
    |> Array.map (fun value ->
        match convertToType arrElemType value with
        | None -> failwith "Could not convert one of given array elements"
        | Some v -> v)
    |> Some

/// Function splits argument string to list of arguments;
/// returns status (bool) and list of arguments (string [])
/// If quantity of splitted values differs from expected quantity then returns (false, [])
let trySplitArgs (argStr: string) (separator: string) (expectedQuantity: int) =
    if argStr = "" || expectedQuantity < 1 then
        raise
            (sprintf "Some of given arguments is wrong \n(arity: %d ; argument-string: \"%s\")" expectedQuantity argStr
             |> ArgumentException)

    let givenArgs = argStr.Split separator
    if givenArgs.Length <> expectedQuantity then false, givenArgs else true, givenArgs

let tryMatchTypesWithArgs (expectedTypes: Type list) (arguments: string list) =
    if expectedTypes.Length <> arguments.Length then
        (false, [])
    else
        let converted =
            List.zip expectedTypes arguments
            |> List.choose (fun (t, strValue) -> convertToType t strValue)

        if converted.Length <> expectedTypes.Length then (false, []) else (true, converted)

let tryMatchTypesWithArgs2 (expectedTypes: Type list) (arguments: string list) =
    let acc = ResizeArray<obj>()

    if expectedTypes.Length <> arguments.Length then
        (false, [])
    else
        List.zip expectedTypes arguments
        |> List.iter (fun (t, strValue) ->
            if t.IsArray then acc.Add(convertToArrayOfType (t.GetElementType()) strValue ";") else acc.Add(convertToType t strValue))

        let converted = Seq.toList acc
        if converted.Length <> expectedTypes.Length then (false, []) else (true, converted)
