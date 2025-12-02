open System.IO
open System.Numerics

let filePath = "2025/02/input.txt"
let input = File.ReadAllText filePath

let parseInput (input : string) =
    input.Split ',' |> Array.collect (fun range ->
        let rangeStartEnd = range.Split '-' |> Array.map BigInteger.Parse
        [| rangeStartEnd.[0]..rangeStartEnd.[1] |]
    )

let rec findRepetition (value : string) size =
    if size < 1 || size > value.Length / 2 then
        false
    else
        if value.Length % size = 0 then
            let repetitionPattern = value.Substring(0, size)
            let repetition = [1..(value.Length / size)] |> Seq.map (fun _ -> repetitionPattern) |> String.concat ""

            if value = repetition then
                true
            else
                findRepetition value (size + 1)
        else
            findRepetition value (size + 1)

// Part 1
let sumInvalidProductIdsWithSingleRepetition productIds =
    productIds
    |> Seq.filter (fun productId ->
        let productIdString = string productId
        if productIdString.Length % 2 = 0 then
            findRepetition productIdString (productIdString.Length / 2)
        else
            false
    )
    |> Seq.sum

let sumOfInvalidProductIdsWithSingleRepetition =
    input
    |> parseInput
    |> sumInvalidProductIdsWithSingleRepetition

printfn "The sum of all invalid product IDs with single repetition is %A" sumOfInvalidProductIdsWithSingleRepetition

// Part 2
let sumInvalidProductIdsWithMultipleRepetitions productIds =
    productIds
    |> Seq.filter (fun productId ->
        let productIdString = string productId
        findRepetition productIdString 1
    )
    |> Seq.sum

let sumOfInvalidProductIdsWithMultipleRepetitions =
    input
    |> parseInput
    |> sumInvalidProductIdsWithMultipleRepetitions

printfn "The sum of all invalid product IDs with multiple repetitions is %A" sumOfInvalidProductIdsWithMultipleRepetitions