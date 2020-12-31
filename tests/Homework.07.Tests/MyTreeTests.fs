module Homework07.Tests.MyTreeTests

open Expecto
open Homework07.MyList
open Homework07.MyTree

[<Tests>]
let BasicTests =
    testList
        "Tests for basic tree functions"
        [ testCase "universe exists"
          <| fun _ ->
              let nodes =
                  MyList.fromList [ Leaf(1)
                                    Leaf(2)
                                    Leaf(3)
                                    Leaf(4) ]

              let actual = TreeNode(value = 0, children = nodes)

              Expect.equal 4 (actual |> MyTreeOfInt.getMax)
              <| "Find max in tree of integer" ]
