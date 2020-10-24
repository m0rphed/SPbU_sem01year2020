/// Provides functions to dynamically invoke any methods
/// of any module in current assembly
module SimpleFunctionsCLI.CliReflection

open System
open System.Reflection

type FunctionInfo =
    { moduleName: string
      name: string
      arity: int
      parameters: (string * Type) list
      printable: string }

/// Returns methods of specified module
let private getModuleMethods moduleName =
    // Get the assembly somehow (by name, by GetEntryAssembly, etc)
    // TODO: add ability to specify/pass assembly to func. as parameter
    // -> use current executing assembly
    let assembly = Assembly.GetExecutingAssembly()

    // Retrieve the methods (including F# functions) on the module type
    match assembly.GetTypes()
          |> Array.tryFind (fun t -> t.Name = moduleName) with
    | None -> [||]
    | Some moduleType -> moduleType.GetMethods()

/// Returns list with all functions (methods)
/// found in specified module.
let private listMethods moduleName =
    query {
        for m in (getModuleMethods moduleName) do
            where (not m.IsHideBySig) // exclude standard methods like `GetType`
            select (m.Name, m.GetParameters())
    }
    |> Seq.toList

/// Returns tuple with all information about function parameters
let private ParamsInfo (info: ParameterInfo []): int * (string * Type) list * string =
    let arity = info.Length
    let mutable _printable = arity |> sprintf "\t\t Arity = %d\n"
    let mutable _params = []
    info
    |> Array.iteri (fun i x ->
        let (_name, _type) = (x.Name, x.ParameterType)
        _params <- _params @ [ (_name, _type) ]
        _printable <-
            _printable
            + (sprintf "\t\t %u) %s : %O\n" (i + 1) _name _type))
    (arity, _params, _printable)

/// Returns formatted string:
/// <function arity>
/// (ParameterNumber) <ParameterName : ParameterType)>
let private getParamsInfo (info: ParameterInfo []): string =
    let mutable signatureMsg = sprintf "\t\t Arity = %d\n" info.Length
    info
    |> Array.iteri (fun i elem ->
        let (_name, _type) = (elem.Name, elem.ParameterType)

        let msg =
            sprintf "\t\t %u) %s : %O\n" (i + 1) _name _type

        signatureMsg <- signatureMsg + msg)
    signatureMsg

/// Returns formatted string with
/// <function number> <function name> <function signature>
let getPrintableListOfFunctions moduleName =
    let mutable formattedMsg = "\nFound:\n\n"
    (listMethods moduleName)
    |> List.iteri (fun i elem ->
        let (fn, _params) = (fst elem, snd elem)

        let msg =
            sprintf "%u) %s \n%s\n" (i + 1) fn (getParamsInfo _params)

        formattedMsg <- formattedMsg + msg)
    formattedMsg

let getFnInfo moduleName fnName: FunctionInfo =
    match listMethods moduleName with
    | [] -> failwith "Specified module has no methods"
    | ms ->
        match List.tryFind (fun (n, _) -> n = fnName) ms with
        | Some (fn, _prmInfo) ->
            let ar, p, msg = ParamsInfo(_prmInfo)
            { moduleName = moduleName
              name = fn
              arity = ar
              parameters = p
              printable = msg }
        | None -> failwith "Specified method was not found"

let isFunctionInModule moduleName fnName =
    match listMethods moduleName with
    | [] -> false
    | functions ->
        match List.tryFind (fun (n, _) -> n = fnName) functions with
        | None -> false
        | Some _ -> true

let parametersNames fnInfo =
    fnInfo.parameters |> List.map (fun x -> fst x)

let parametersTypes fnInfo =
    fnInfo.parameters |> List.map (fun x -> snd x)

let getParametersTypes moduleName fnName =
    let fnInfo = getFnInfo moduleName fnName
    fnInfo.parameters |> List.map (fun x -> snd x)

let tryGetFunctionNameByNumber md number =
    let methods = listMethods md
    if number > methods.Length then (false, "")
    else (true, fst (methods.[number - 1]))

/// Finds and invokes function (method) by given name in specified module.
/// Function parameters should be specified as `obj []`
let invoke moduleName methodName parameters =
    match getModuleMethods moduleName
          |> Array.tryFind (fun f -> f.Name = methodName) with
    | Some f -> f.Invoke(null, parameters) // Invoke the function
    | None -> failwith "Specified method was not found"
