/// Provides colorful console output
module SimpleFunctionsCLI.CliColors

open System

/// Simple log function - locks the console,
/// then prints given message to the console; adds new line
let logFn =
    // using lock object for the sake of thread safety
    let lockObj = obj ()
    // print msg -- given log message
    fun color msg ->
        lock lockObj (fun _ ->
            Console.ForegroundColor <- color
            printfn "%s" msg
            Console.ResetColor())

/// Prints given message to the console
let logF =
    let lockObj = obj ()
    fun color msg ->
        lock lockObj (fun _ ->
            Console.ForegroundColor <- color
            printf "%s" msg
            Console.ResetColor())

// use different colors in different cases
let ok = logFn ConsoleColor.Green
let info = logFn ConsoleColor.Magenta
let warn = logFn ConsoleColor.Yellow
let error = logFn ConsoleColor.Red
