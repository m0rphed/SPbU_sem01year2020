module Homework08.Main

// use square regions to define matrix of size [N x N]
open Homework08.QuadTreeTypes.SquareRegion
// use PR QuadTree
open Homework08.QuadTree

[<EntryPoint>]
let main _ =
    let N = 8
    let matrixAsSquare = Square((N / 2, N / 2), N / 2)
    let qt = QuadTree.initQuadTree (matrixAsSquare)

    if qt |> QuadTree.insert ((1.0, 1.0)) "42"
    then printfn "%A\n" qt

    if qt |> QuadTree.insert ((2.0, 1.0)) "69"
    then printfn "%A\n" qt

    if qt |> QuadTree.remove ((2.0, 1.0)) then printfn "%A\n" qt

    if qt |> QuadTree.insert ((2.0, 1.0)) "73"
    then printfn "%A\n" qt

    if qt |> QuadTree.insert ((1.0, 0.0)) "42 42"
    then printfn "%A\n" qt

    qt.ToString()
    |> printfn "Test ToString() implementation:\n%s\n"

    0
