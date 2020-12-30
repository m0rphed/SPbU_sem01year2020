module Homework03.Main

open System
open FibonacciSimple

let printResults num =
    printfn "\nLets calculate %i fib number" num
    printfn "Task-1: %A" (fibonacci num)
    printfn "Task-2: %A" (fibonacciIterative num)
    printfn "Task-3: %A" (fibonacci' num)
    printfn "Task-4: %A" (fibonacciViaMatrix num)
    printfn "Task-5: %A" (fibonacciViaMatrix' num)

let printBigFibonacciNumber num =
    printfn "\nLets calculate %i fib number" num
    printfn "Task-4: %A" (fibonacciViaMatrix num)
    printfn "Task-5: %A" (fibonacciViaMatrix' num)

let printFibSequence num =
    printfn "\nLets calculate %i fib sequence" num
    let initial = [1I; 1I]
    let fibs = fibonacciSequence initial
    printfn "Task-6: %A" ((Seq.take num fibs) |> Seq.toList)

/// Prints calculated sequence (using alternative solution for task-6)
let printFibSequence' num =
    printfn "\nLets calculate %i fib sequence (using alternative solution)" num
    let fibs = fibonacciSequence' num
    printfn "Task-6: %A" ((Seq.take num fibs) |> Seq.toList)

[<EntryPoint>]
let main _ =
    printResults 1
    printResults 2
    printResults 3
    printResults 13
    printResults 30
    printBigFibonacciNumber 100
    printBigFibonacciNumber 1000
    printFibSequence 100
    printFibSequence' 100
    0
