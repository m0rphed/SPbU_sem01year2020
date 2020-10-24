/// Implementation of Homework 02
module SimpleFunctionsCLI.Homework2

open System

// Вспомогательная private функция
/// (RUS): Функция возведения в степень для целых чисел.
///
/// (ENG): Exponentiation function for integers.
let rec private pow number g =
    match g with
    | 0 -> 1
    | g when g % 2 = 1 -> number * (pow number (g - 1))
    | _ -> let p = pow number (g / 2) in p * p

// -> Задача 1
/// (RUS): Функция вычисляющая значение
/// выражения x^4+x^3+x^2+x+1 "наивным" способом.
///
/// (ENG): Calculates value of the
/// expression x^4+x^3+x^2+x+1
/// in a "naive" way.
let polynomialNaive x =
    (pow x 4) + (pow x 3) + (pow x 2) + x + 1

// -> Задача 2
/// (RUS): Функция вычисляющая значение
/// выражения x^4+x^3+x^2+x+1 используя минимальное
/// число умножений и сложений.
///
/// (ENG): Calculates value of the
/// expression x^4+x^3+x^2+x+1 using the minimum
/// number of multiplications and additions.
let polynomialEfficient x =
    let t = x * x
    (t + x) * (t + 1) + 1

// -> Задача 3
/// (RUS): Функция возвращает индексы тех элементов массива,
/// которые не больше, чем заданное число.
///
/// (ENG): Returns indices of array elements
/// that are less than or equal to a given number.
let indexesLessOrEqual (boundary: int) (array: int array) =
    array
    |> Array.indexed
    |> Array.filter (fun (_i, x) -> x <= boundary)
    |> Array.map fst

// -> Задача 4
/// (RUS): Функция возвращает индексы элементов массива,
/// лежащих вне диапазона (диапазон != интервал т.е. не вкл. крайние значения), заданного двумя числами.
///
/// (ENG): Returns indices of array elements outside
/// the range (range != interval) specified by two given numbers.
let indexesNotInRange (left: int) (right: int) (array: int array): int array =
    if left >= right then failwith "Range was set incorrectly" // "Диапазон задан некорректно"
    array
    |> Array.indexed
    |> Array.filter (fun (_i, x) -> not (left < x && x < right))
    |> Array.map fst

/// (RUS): Функция возвращает индексы элементов массива,
/// лежащих вне интервала (т.е. вкл. крайние значения), заданного двумя числами.
///
/// (ENG): Returns indices of array elements outside
/// the interval specified by two given numbers.
let indexesNotInInterval (left: int) (right: int) (array: int array): int array =
    if left >= right then failwith "Range was set incorrectly" // "Диапазон задан некорректно"
    array
    |> Array.indexed
    |> Array.filter (fun (_, x) -> not (left <= x && x <= right))
    |> Array.map fst

// -> Задача 5
/// (RUS): Функция меняет местами нулевой и первый элементы,
/// не используя дополнительной переменных.
///
/// (ENG): Swaps zero and the first elements
/// without using additional variables.
let swapInAtomicArray (xs: int array): int array =
    if xs.Length > 2 then raise (ArgumentException("Array is not atomic (length > 2)"))
    xs.[0] <- xs.[1] + xs.[0]
    xs.[1] <- xs.[0] - xs.[1]
    xs.[0] <- xs.[0] - xs.[1]
    xs

// -> Задача 6
/// (RUS): Функция меняет местами i-ый и j-ый элементы,
/// не используя дополнительной памяти/переменных.
///
/// (ENG): Swaps the i-th and j-th elements,
/// without using additional variables.
let swapInArray (xs: int array) (i: int) (j: int): int array =
    xs.[i] <- xs.[j] + xs.[i]
    xs.[j] <- xs.[i] - xs.[j]
    xs.[i] <- xs.[i] - xs.[j]
    xs
