module Homework07.MyList

(* <Task Description / Задание> [Russian]:
    1. (1 балл) Реализовать самостоятельно полиморфный непустой список (далее будем называть этот список - MyList).
        Реализовать для него функции:
            - сортировки,
            - вычисления длины,
            - конкатенации,
            - map,
            - iter.
        + Реализовать преобразование из стандартного списка в MyList.

    2. (1 балл) На основе MyList реализовать MyString, представляющий строку как список символов (тип char).
        Реализовать преобразование стандартной строки в MyString и конкатенацию строк для MyString.

    3. (1 балл) Реализовать тип дерева MyTree с произвольным количеством потомков в каждом узле (использовать MyList).
        Каждый узел должен хранить данные произвольного типа.

    4. (1 балл) Пусть есть MyTree, хранящий в узлах целые числа.
        Реализовать функции, которые находят
            => максимальный хранимый элемент,
            => среднее значение всех хранимыхэлементов.
*)

/// Generic List implementation "from scratch";
/// `MyList` must be nonempty (always contains a value)
type MyList<'T> =
    | SingleValue of 'T
    | Node of value: 'T * next: MyList<'T>

/// Implementation of std. list function for MyList
module MyList =
    let rec iter func list =
        match list with
        | SingleValue v -> func v
        | Node (v, nextNode) ->
            func v
            nextNode |> iter func

    /// Map implemented for `MyList`
    let rec map func list =
        match list with
        | SingleValue v -> SingleValue(func v)
        | Node (v, nextNode) -> Node(func v, map func nextNode)

    /// Fold implemented for MyList
    let rec fold folder acc list =
        match list with
        | SingleValue v -> folder acc v
        | Node (v, nextNode) ->
            nextNode
            |> fold folder (folder acc v)

    /// Returns length of the list
    let length list = fold (fun acc _elem -> acc + 1) 0 list

    /// Combines two lists of the same type into one
    let rec concat listA listB =
        match listA with
        | SingleValue v -> Node(v, listB)
        | Node (v, nextNode) -> Node(v, concat nextNode listB)

    /// Converts from list to nonempty MyList
    let rec fromList list =
        match list with
        | [ v ] -> SingleValue v
        | head :: tail -> Node(head, fromList tail)
        | [] -> failwith "Could not construct nonempty MyList from an empty std. list"

    /// Converts MyList to nonempty list
    let rec toList myList =
        match myList with
        | SingleValue v -> [ v ]
        | Node (v, nextNode) ->
            nextNode
            |> fold (fun listAcc currentElement -> listAcc @ [ currentElement ]) [ v ]


    /// Quicksort implemented for MyList
    let qsort comparer myList =
        let rec sort list =
            match list with
            | SingleValue theTail -> SingleValue theTail
            | Node (v, nextNode) ->
                let previous = v

                match nextNode with
                | Node (nodeValue, nodeAfterThat) ->
                    let current = nodeValue
                    let tail = nodeAfterThat

                    if comparer previous current
                    then Node(current, sort (Node(previous, tail)))
                    else Node(previous, sort (Node(current, tail)))

                | SingleValue current ->
                    if comparer previous current then
                        Node(current, SingleValue previous)
                    else
                        Node(previous, SingleValue current)

        let mutable res = myList
        for i = 0 to (length myList) - 1 do
            res <- sort res

        res
