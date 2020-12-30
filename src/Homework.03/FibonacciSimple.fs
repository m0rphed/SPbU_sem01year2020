module Homework03.FibonacciSimple

(*
<Task Description / Задание> [Russian]:
    1. Реализовать вычисление n-ого числа Фибоначчи рекурсивным методом.
    2. Реализовать вычисление n-ого числа Фибоначчи итеративным методом.
    3. Реализовать вычисление n-ого числа Фибоначчи используя хвостовую рекурсию
        (не используя mutable и других изменяемых структур).
        Подсказка: нужно использовать рекурсию с аккумулятором.
    4. Реализовать вычисление n-ого числа Фибоначчи через перемножение матриц “наивным” методом.
        Функции построения единичной матрицы, умножения и возведения
        в степень должны быть реализованы в общем виде.
    5. Реализовать вычисление n-ого числа Фибоначчи через перемножение матриц за логарифм.
    6. Реализовать вычисление всех чисел Фибоначчи до n-ого включительно.
    = = = = = = =
    P.S. Для всех задач обеспечить чтение n из консоли и печать результата в консоль
*)

// functions (identityMatrix, matrixMultiply, powMatrix) used with 2D array
// to work with them as with matrices
open MatrixFunctions

// This function exists only to reduce boilerplate where possible
/// Throws ArgumentException if passed number <= 0
let inline private throwExceptionOnIncorrectInput n =
    if n <= 0
    then invalidArg "number" "Number must be >= 0"

/// Task 1
/// recursive implementation
/// (for consistency this function
/// accepts number of type int32, but returns System.Numerics.BigInteger;
/// cause we assume that the N-th member of the Fibonacci sequence
/// is significantly larger than its ordinal number - N)
let rec fibonacci (number: int) =
    number |> throwExceptionOnIncorrectInput // check input
    /// Wraps recursive implementation
    let rec innerFib bigNumber =
        if bigNumber <= 2I
        then 1I
        else innerFib (bigNumber - 2I) + innerFib (bigNumber - 1I)

    innerFib (number |> bigint)

/// Task 3
/// tail-recursive (more efficient) implementation
let fibonacci' number =
    number |> throwExceptionOnIncorrectInput
    // a, b used as "accumulators"
    // and countdown at the beginning is set to N-th fibonacci number;
    // then countdown value decreases
    /// Wraps fibonacci function
    let rec fib a b countdown =
        match countdown with
        | 0 -> a
        | 1 -> b
        | n -> (fib b (a + b) (n - 1))

    fib (0 |> bigint) (1 |> bigint) number

/// Task 2
/// Iterative
let fibonacciIterative number =
    number |> throwExceptionOnIncorrectInput

    if number < 2 then // if number is 0 or 1 -> 0I or 1I is the answer
        number |> bigint
    else
        let mutable prevFib, fib = 0I, 1I
        let mutable i, temp = 2, 0I

        while (i <= number) do
            temp <- prevFib
            prevFib <- fib
            fib <- temp + fib
            i <- i + 1

        fib

/// Task 4
/// Fibonacci number using "naive" matrix-exponentiation
let fibonacciViaMatrix number =
    number |> throwExceptionOnIncorrectInput
    let mutable result = array2D [ [ 0I; 1I ]; [ 1I; 1I ] ]
    // make a "shallow copy"
    let m = Array2D.copy result

    for i = 2 to number do
        result <- multiplyMatrices result m

    result.[0, 1]

/// Task 5
/// Fibonacci number using matrix-exponentiation (more efficient)
let fibonacciViaMatrix' number =
    number |> throwExceptionOnIncorrectInput

    let result =
        array2D [| [| 0I; 1I |]; [| 1I; 1I |] |]
        |> powMatrix number
    result.[0, 1]

/// Task 6
let fibonacciSequence (initialValues: list<bigint>) =
    let func =
        (function
        | least :: rest ->
            let this = least + Seq.reduce (+) rest
            Some(this, rest @ [ this ])
        | _ -> None)

    Seq.append (initialValues |> Seq.ofList) (func |> Seq.unfold <| initialValues)

/// Alternative solution for Task 6
let fibonacciSequence' n =
    n |> throwExceptionOnIncorrectInput

    let getSequence =
        Seq.unfold (fun (x, y) -> Some(x, (y, x + y))) (0I, 1I)

    getSequence |> Seq.take n
