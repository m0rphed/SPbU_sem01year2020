module Homework2Tests

open Expecto
open SimpleFunctionsCLI.Homework2

[<Tests>]
let testsForBL =
  testList "Naive tests for functions from Homework2" [

    testCase "Task 1" <| fun _ ->
      "(2)^4 + (2)^3 + (2)^2 + (2) + 1 = 31"
        |> Expect.equal 31 (polynomialNaive 2)
      "(1)^4 + (1)^3 + (1)^2 + (1) + 1 = 5"
        |> Expect.equal 5 (polynomialNaive 1)

    testCase "Task 2" <| fun _ ->
      "(2)^4 + (2)^3 + (2)^2 + (2) + 1 = 31"
        |> Expect.equal 31 (polynomialEfficient 2)
      "(1)^4 + (1)^3 + (1)^2 + (1) + 1 = 5"
        |> Expect.equal 5 (polynomialEfficient 1)

    testCase "Task 3" <| fun _ ->
      let result = [|1; 2; 3; 4|] |> indexesLessOrEqual 2
      "([1; 2; ..] <= 2) so the result indexes are 0; 1"
        |> Expect.equal [|0; 1;|] result

    testCase "Task 4 (range)" <| fun _ ->
      let result = [|-1; 0; 1; 2; 3; 4; 5; 6; 7;|] |> indexesNotInRange 1 6
      "Expected {id:value} -> [ 0:-1; 1:0; 2:1; 7:6; 8:7 ]"
        |> Expect.equal [|0; 1; 2; 7; 8|] result

    testCase "Task 4 (interval)" <| fun _ ->
      let result = [|-1; 0; 1; 2; 3; 4; 5; 6; 7;|] |> indexesNotInInterval 1 6
      "Expected {id:value} -> [ 0:-1; 1:0; 8:7 ]"
        |> Expect.equal [|0; 1; 8|] result

    testCase "Task 5" <| fun _ ->
      let result = [|0; 1|] |> swapInAtomicArray
      "[|0; 1|] => [|1; 0|]"
        |> Expect.equal [|1; 0|] result

    testCase "Task 6" <| fun _ ->
      let result = swapInArray [|-1; 2; 3; -4|] 0 3
      "[|-1; 2; 3; -4|] => [|-4; 2; 3; -1|]"
        |> Expect.equal [|-4; 2; 3; -1|] result
  ]
