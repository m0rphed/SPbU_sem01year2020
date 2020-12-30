namespace Homework03.FibonacciAdditional

module MemoizedSimple =
    open System.Collections.Generic

    let rec fibonacciMemoized number =
        let memo = Dictionary<_, _>()

        let rec fibInner =
            function
            | n when n = 0I -> 0I
            | n when n = 1I -> 1I
            | n ->
                fibonacciMemoized (n - 1I)
                + fibonacciMemoized (n - 2I)

        if memo.ContainsKey(number) then
            memo.[number]
        else
            let res = fibInner number
            memo.[number] <- res
            res

module MemoizedConcurrent =
    open System.Collections.Concurrent

    let memo = ConcurrentDictionary<'a, 'b>()

    let memoize (f: 'a -> 'b) arg = memo.GetOrAdd(arg, f)

    let mapAsync (f: 'a -> 'b) (s: seq<'a>) =
        seq {
            for element in s do
                yield async { return f element }
        }

    let rec fibMemoized =
        memoize
        <| fun number ->
            if number < 2I then
                number
            else
                (fibMemoized (number - 1I)
                 + fibMemoized (number - 2I))

    let run (f: bigint -> bigint) (range: (option<bigint> * bigint)) =
        match (fst range) with
        | None -> [ 0I .. (snd range) ]
        | Some n -> [ n .. snd range ]
        |> mapAsync f
        |> Async.Parallel
        |> Async.RunSynchronously
