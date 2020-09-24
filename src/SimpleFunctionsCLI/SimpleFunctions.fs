module SimpleFunctionsCLI.SimpleFunctions

open System

/// Функция возведения в степень для целых чисел
let rec private pow number g =
    match g with
    | 0 -> 1
    | g when g % 2 = 1 -> number * (pow number (g - 1))
    | _ ->
        let p = pow number (g / 2) in p * p

// -> Задача 1
/// Функция вычисляющая значение
/// выражения x^4+x^3+x^2+x+1 "наивным" способом
let polynomialNaiveCalculation x = (pow x 4) + (pow x 3) + (pow x 2) + x + 1

// -> Задача 2
/// Функция вычисляющая значение
/// выражения x^4+x^3+x^2+x+1 исп. минимальное число умножений и сложений
let polynomialEfficientCalculation x =
    let t = x * x
    (t + x) * (t + 1) + 1

// -> Задача 3
/// Функция возвращает индексы тех элементов массива,
/// которые не больше, чем заданное число
let indexesOfElementsLessOrEqual array boundary =
    array
    |> Array.indexed
    |> Array.filter (fun (_i, x) -> x <= boundary)
    |> Array.map fst

// -> Задача 4
/// Функция возвращает индексы элементов массива,
/// лежащих вне диапазона, заданного двумя числами
let indexesOfElementsNotInRange (array: int array) (left: int) (right: int): int array =
    if left >= right then failwith "Диапазон задан некорректно"
    array
    |> Array.indexed
    |> Array.filter (fun (_i, x) -> not (left > x || x > right))
    |> Array.map fst

// -> Задача 5
/// Функция меняет местами нулевой и первый элементы,
/// не используя дополнительной памяти/переменных.
let smallestArrayElementSwap arrayOfTwo =
    raise (NotImplementedException())

// -> Задача 6
let arrayElementSwap array i j =
    raise(NotImplementedException())
