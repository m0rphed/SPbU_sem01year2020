module Homework08.QuadTreeTypes.SquareRegion

/// Implements direction for 2D space
type Direction =
    | NW
    | NE
    | SW
    | SE

/// Square (with coordinates, height and width) with type defined with
/// * a point (repr. tuple of coordinates) as a center
/// * a half dimension
[<Struct>]
type Square =
    val center: float * float
    val halfDimension: float

    new(center: int * int, halfDim: int) =
        { center = (fst center) |> float, (snd center) |> float
          halfDimension = halfDim |> float }

    new(center, halfDim) =
        { center = center
          halfDimension = halfDim }

    /// Checks that the point is inside the current square
    member this.Contains(X, Y) =
        match abs (Y - snd this.center) <= this.halfDimension, abs (X - fst this.center) <= this.halfDimension with
        | true, true -> true
        | _, _ -> false

    /// Returns subregion (sub-square) by given direction
    member this.Split(direction: Direction) =
        let quarter = this.halfDimension / 2.0

        match direction with
        | NW -> Square((fst this.center - quarter, snd this.center - quarter), quarter)
        | SW -> Square((fst this.center - quarter, snd this.center + quarter), quarter)
        | NE -> Square((fst this.center + quarter, snd this.center - quarter), quarter)
        | SE -> Square((fst this.center + quarter, snd this.center + quarter), quarter)

    /// Returns direction in the region traversal for a point (tuple of int)
    member this.GetDirection(X, Y) =
        match Y, X with
        | y, x when y >= snd this.center && x >= fst this.center -> SE
        | y, x when y >= snd this.center && x < fst this.center -> SW
        | y, x when y < snd this.center && x >= fst this.center -> NE
        | _, _ -> NW

    override this.ToString() =
        let dim = this.halfDimension * 2.

        "square ["
        + dim.ToString()
        + " x "
        + dim.ToString()
        + "], "
        + this.center.ToString()
