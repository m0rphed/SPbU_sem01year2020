module Homework08.QuadTree

// use Square Region type
open Homework08.QuadTreeTypes.SquareRegion

/// Validate input value
let private validate v =
    match v with
    | Some v -> v.ToString()
    | None -> "< empty >"

/// QuadTree node of Point-Region QuadTree
type QuadNode<'tValue> =
    { NW: QuadTree<'tValue> option
      NE: QuadTree<'tValue> option
      SE: QuadTree<'tValue> option
      SW: QuadTree<'tValue> option }
    override node.ToString() =
        "\n{\n    NW: "
        + validate node.NW
        + "\n    NE: "
        + validate node.NE
        + "\n    SE: "
        + validate node.SE
        + "\n    SW: "
        + validate node.SW
        + " }"

/// Point-Region QuadTree implementation (with square as region)
and QuadTree<'tValue> =
    { region: Square
      mutable value: 'tValue option
      mutable node: QuadNode<'tValue> option }
    override tree.ToString() =
        "{\n  region: "
        + tree.region.ToString()
        + "\n  value: "
        + validate tree.value
        + "\n  nodes: "
        + validate tree.node
        + " }"

/// Functions for working with Point-Region QuadTree
module QuadTree =
    let private getEmptyNode =
        { NW = None
          NE = None
          SW = None
          SE = None }

    let private getSubTree (dir: Direction) node =
        match node with
        | None -> None
        | Some node ->
            match dir with
            | NW -> node.NW
            | NE -> node.NE
            | SW -> node.SW
            | SE -> node.SE

    let initQuadTree region =
        { region = region
          value = None
          node = None }

    let private createSubTree dir tree = initQuadTree (tree.region.Split dir)

    let private initializeNode node =
        match node with
        | Some node -> node
        | None -> getEmptyNode

    let private createNode dir subTree tree =
        let node = initializeNode tree.node

        match dir with
        | NW -> tree.node <- Some { node with NW = Some subTree }
        | NE -> tree.node <- Some { node with NE = Some subTree }
        | SW -> tree.node <- Some { node with SW = Some subTree }
        | SE -> tree.node <- Some { node with SE = Some subTree }

    let private subdivide (dir: Direction) tree =
        match tree.node |> getSubTree dir with
        | Some subTree -> subTree
        | None ->
            let subTree = createSubTree dir tree
            tree |> createNode dir subTree
            subTree

    /// <summary>
    /// Inserts a point (with associated value) in a QuadTree
    /// </summary>
    /// <returns>
    /// True if point was inserted in QuadTree
    /// False otherwise
    /// </returns>
    let rec insert (x, y) value tree =
        match tree.region.Contains(x, y) with
        | false -> false
        | true ->
            match tree.region.halfDimension < 1. with
            | true ->
                match tree.value with
                | Some _ -> false
                | None -> // do insert only if no value was assigned at the point(x, y)
                    tree.value <- Some value
                    true
            | false ->
                let direction = tree.region.GetDirection(x, y)

                match tree.node with
                | None ->
                    tree
                    |> subdivide direction
                    |> insert (x, y) value
                | Some _ as children ->
                    match children |> getSubTree direction with
                    | Some subTree ->
                        subTree |> insert (x, y) value
                    | None ->
                        tree
                        |> subdivide direction
                        |> insert (x, y) value

    /// <summary>
    /// Inserts a point (with assoc. value) with INTEGER coordinates in a QuadTree
    /// </summary>
    /// <returns>
    /// True if point was inserted in QuadTree
    /// False otherwise
    /// </returns>
    let insertByIntCoord (x: int, y: int) value tree =
        let X, Y = float x, float y
        insert (X, Y) (value: 'tValue) tree

    /// <summary>
    /// Removes a point from a QuadTree
    /// </summary>
    /// <returns>
    /// True if point was removed from QuadTree
    /// False otherwise
    /// </returns>
    let rec remove (x, y) tree =
        match tree.region.Contains(x, y) with
        | false -> false
        | true ->
            match tree.value with
            | Some _ ->
                tree.value <- None
                true
            | None ->
                // get direction of search
                let subTree =
                    tree.node
                    |> getSubTree (tree.region.GetDirection(x, y))

                match subTree with
                | Some subTree -> subTree |> remove (x, y)
                | None -> false
