module SimpleFunctionsCLI.ArgumentParser

open System
open Argu

open CliColors
open CliReflection
open ValueConversion

/// Defines all CLI args
type CliArguments =
    | List_Tasks // get correct function names and arguments
    | Task_Name of taskName: string * parameters: string // run task by specified name
    | Task_Number of taskNumber: int * parameters: string // run task by specified name
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | List_Tasks -> "print list of all tasks"
            | Task_Name _ -> "call function by name"
            | Task_Number _ -> "call function by number"
            //| Interactive_Mode _ -> "start CLI in interactive mode" //TODO: implement CLI interactive mode

let runTaskByName md name argList =
    let arr1 = tryMatchTypesWithArgs (getParametersTypes md name) (argList |> Array.toList)
    //let arr2 = tryMatchTypesWithArgs2 (getParametersTypes _module taskName) (argList |> Array.toList)
    match tryMatchTypesWithArgs (getParametersTypes md name) (Array.toList argList) with
    | true, argsObj ->
        try
            let result =
                invoke md name (argsObj |> List.toArray)

            "=> Result:" |> logFn ConsoleColor.DarkCyan
            result |> sprintf "  %A"
            |> logFn ConsoleColor.DarkGreen

        with :? Exception as ex ->
            sprintf "Error running %s: %s" name ex.Message 
            |> error
    | false, _ ->
        error
            (sprintf "Failed to run < %s >\nProvided arguments could not be converted to expected types"
                 name)

let printFunctions md =
        sprintf "There are all available functions from <%s>" md
        |> info
        (getPrintableListOfFunctions md)
        |> printfn "%s"

let printGivenArgs name argStr =
    // print given task name
    info "\nTrying to find task with name:"
    sprintf "  %s" name |> logFn ConsoleColor.Blue
    // print given argument string
    logFn ConsoleColor.DarkCyan "\n=> Arguments:"
    sprintf "  %s" argStr
    |> logFn ConsoleColor.DarkYellow

[<EntryPoint>]
let main argv =
    try
        let parser =
            ArgumentParser.Create<CliArguments>(programName = "SimpleFunctionsCLI")
        // get parsed data from Argu
        let results = parser.Parse argv
        let args = parser.ParseCommandLine argv

        // define name of the module which contains all business logic
        let _inModule = "Homework2"
        let _cliArgSeparator = ","

        if results.Contains(List_Tasks) then
            printFunctions _inModule

        elif results.Contains(Task_Name) then
            // get task name with given argument string
            let task, argumentString = args.GetResult(Task_Name)
            // print beautiful message to show given arguments
            printGivenArgs task argumentString
            // get all meta-information about this task
            let f = getFnInfo _inModule task
            let success, argList =
                trySplitArgs argumentString _cliArgSeparator f.arity

            // check if string values match expected arity of the task (function)
            if not success then
                logFn ConsoleColor.DarkRed "=> Could not parse task arguments:\n"
                error "Parameters count mismatch!"
            else
                // if arity matched try to run this task
                runTaskByName _inModule task argList

        elif results.Contains(Task_Number) then
            // get task number with given argument string
            let number, argumentString = args.GetResult(Task_Number)
            // print beautiful message to show given arguments
            printFunctions _inModule           

            // guess name by number
            let success, name = number |> tryGetFunctionNameByNumber _inModule
            if not success then
                "No such function with number: " + number.ToString() |> error
            else
                // print task number and guessed name
                logF ConsoleColor.DarkYellow "=> Selected: "
                number.ToString() |> logFn ConsoleColor.DarkMagenta
                printf "  matches "
                name |> logFn ConsoleColor.Cyan
                // print beautiful message to show given arguments
                printGivenArgs name argumentString
                let f = getFnInfo _inModule name
                let success, argList =
                    trySplitArgs argumentString _cliArgSeparator f.arity

                if not success then
                    logFn ConsoleColor.DarkRed "=> Could not parse task arguments:\n"
                    error "Parameters count mismatch!"
                else
                    runTaskByName _inModule name argList

        0 // return an 'OK' integer exit code
    with :? Argu.ArguParseException as ex ->
        printfn "%s" ex.Message
        1 // return an 'Error' exit code
