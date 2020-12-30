module Homework03.FibonacciSimpleTests

open Expecto
open Homework03.FibonacciSimple

[<Tests>]
let fibonacciNthNumberTests =
    let upperBound = 30

    testList
        "Test properties for tasks 1 -- 5"
        [ testProperty "Task 1 and 2"
          <| fun (n: int) ->
              if 0 < n && n <= upperBound then
                  Expect.equal (fibonacci (abs n)) (fibonacciIterative (abs n))
                  <| "Results for recursive and iterative fibonacci implementations should be equal"

          testProperty "Task 2 and 3"
          <| fun (n: int) ->
              if 0 < n && n <= upperBound then
                  Expect.equal (fibonacciIterative (abs n)) (fibonacci' (abs n))
                  <| "Results for iterative and tail-recursive fibonacci implementations should be equal"

          testProperty "Task 3 and 4"
          <| fun (n: int) ->
              if 0 < n && n <= upperBound then
                  Expect.equal (fibonacci' (abs n)) (fibonacciViaMatrix (abs n))
                  <| "Results for tail-recursive and 'via matrix' fibonacci implementations should be equal"

          testProperty "Task 4 and 5"
          <| fun (n: int) ->
              if 0 < n && n <= upperBound then
                  Expect.equal (fibonacciViaMatrix (abs n)) (fibonacciViaMatrix' (abs n))
                  <| "Results for 'via matrix' and 'via matrix with Log(n) complexity' fibonacci implementations should be equal" ]


