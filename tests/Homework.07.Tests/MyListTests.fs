module Homework07.Tests.MyListTests

open Expecto
open Homework07.MyList

[<Tests>]
let BasicTests =
    let myList = Node(1, Node(2, Node(3, SingleValue 4)))

    testList
        "Tests for basic list functions (length, iter, map, fold, fromList, toList)"
        [

          testCase "Test length"
          <| fun _ ->
              Expect.equal (MyList.length myList) 4
              <| "Length of [1; 2; 3; 4] is 4"

          testCase "Test iter"
          <| fun _ ->
              let mutable actualList = []

              myList
              |> MyList.iter (fun value -> actualList <- actualList @ [ value ])

              Expect.equal actualList (MyList.toList myList)
              <| "Iterate over MyList and save all values to std. list; then compare results"

          testCase "Test map"
          <| fun _ ->
              let square = fun value -> value * value

              let expected =
                  Node(1, Node(4, Node(9, SingleValue 16)))

              Expect.equal (myList |> MyList.map square) expected
              <| "Maps MyList with square function; then compare result to squared values"

          testCase "Test fold"
          <| fun _ ->
              let expected =
                  myList
                  |> MyList.toList
                  |> List.fold (fun acc elem -> acc + elem) 0

              let actual =
                  myList |> MyList.fold (fun acc v -> acc + v) 0

              Expect.equal actual expected
              <| "Fold MyList with sum function; then compare result to expected sum"

          testCase "Test fromList"
          <| fun _ ->
              Expect.equal (MyList.fromList [ 1; 2; 3; 4 ]) myList
              <| "I compute, therefore I am."

          testCase "Test toList"
          <| fun _ ->
              let expected = [ 1; 2; 3; 4 ]

              Expect.equal (MyList.toList myList) expected
              <| "I compute, therefore I am." ]

[<Tests>]
let ListSortingTests =
    testList
        "Tests for sorting (quicksort)"
        [ testCase "Test quicksort"
          <| fun _ ->
              let expected = MyList.fromList [ 1; 2; 3; 4 ]

              let actual =
                  MyList.fromList [ 4; 1; 3; 2 ]
                  |> MyList.qsort (fun x y -> x > y)

              Expect.equal actual expected
              <| "I compute, therefore I am." ]
