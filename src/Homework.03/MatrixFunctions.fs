module Homework03.MatrixFunctions

/// Returns identity matrix represented as Array2D of type 'a
let inline identityMatrix n =
    let matrix = Array2D.zeroCreate n n

    for i = 0 to n - 1 do
        for j = 0 to n - 1 do
            if i = j
            then matrix.[i, j] <- LanguagePrimitives.GenericOne<'a>

    matrix

/// Multiplies two matrices of type 'a (matrix product)
let inline multiplyMatrices m1 m2 =
    let rows1, cols1 = Array2D.length1 m1, Array2D.length2 m1
    let rows2, cols2 = Array2D.length1 m2, Array2D.length2 m2

    if cols1 <> rows2 then
        "Could not multiply matrix because of dimension mismatch"
        |> invalidArg "m2"

    let result = Array2D.zeroCreate rows1 cols2

    for i = 0 to rows1 - 1 do
        for j = 0 to cols2 - 1 do
            for h = 0 to cols1 - 1 do
                result.[i, j] <- result.[i, j] + m1.[i, h] * m2.[h, j]

    result

/// Returns matrix in power of N
/// (matrix is represented as an Array2D of type 'a)
let inline powMatrix n (matrix: 'a [,]) =
    /// `_powRec` function solves problems with generalization of matrix type
    /// (so `powMatrix` could be generic function)
    let rec _powRec n (matrix: 'a [,]) =
        if n = 0 then
            identityMatrix 2
        elif n % 2 = 0 then
            let m = _powRec (n / 2) matrix
            multiplyMatrices m m
        else
            let m = _powRec (n - 1) matrix
            multiplyMatrices m matrix

    _powRec n matrix
