module Homework07.MyTree

// use MyList
open Homework07.MyList

/// Implementation of Tree
type MyTree<'T> =
    | Leaf of 'T
    | TreeNode of value: 'T * children: MyList<MyTree<'T>>

/// Functions for MyTree<'T>
module MyTree =
    let rec traverseFold acc func (tree: MyTree<'T>) =
        match tree with
        | Leaf v -> func acc v
        | TreeNode (value, children) ->
            MyList.fold (fun acc' -> traverseFold acc' func) (func acc value) children

/// Functions for MyTree<int> (tree which which contains only integer values)
module MyTreeOfInt =
    /// Returns max integer found in MyTree<int>
    let getMax tree =
        tree
        |> MyTree.traverseFold System.Int32.MinValue (max)

    /// Returns the average calculated from the integer values inside MyTree<int>
    let getAverage tree =
        let elemSum, quantity =
            tree
            |> MyTree.traverseFold (0, 0) (fun (sum, quantity) x -> (sum + x), (quantity + 1))

        (float elemSum) / (float quantity)
