open System.IO
open System.Numerics

let filePath = "2025/05/input.txt"
let input = File.ReadAllLines filePath

let parseInput (input : string array) =
    input
    |> Array.filter (fun row -> row <> "")
    |> Array.partition (fun row -> row.Contains '-')
    |> fun (rangeLines, elementLines) ->
        let ranges =
            rangeLines
            |> Array.map (fun r ->
                let rangeValues = r.Split '-' |> Array.map BigInteger.Parse
                rangeValues.[0], rangeValues.[1]
            )
        let elements =
            elementLines
            |> Array.map BigInteger.Parse
        ranges, elements

let ranges, elements = parseInput input

// Part 1
let isInAnyRange ranges element =
    ranges |> Seq.exists (fun (min, max) -> min <= element && element <= max)

let numberOfElementsInRange =
    elements
    |> Seq.filter (isInAnyRange ranges)
    |> Seq.length

printfn "Number of fresh ingredient IDs: %i" numberOfElementsInRange

// Part 2
let mergeRanges ranges =
    ranges
    |> Seq.sortBy fst
    |> Seq.fold (fun merged (mn, mx) ->
        match merged with
        | [] -> [ mn, mx ]
        | (lastMin, lastMax) :: rest when mn <= lastMax + 1I ->
            (lastMin, max lastMax mx) :: rest
        | _ ->
            (mn, mx) :: merged
    ) []
    |> List.rev

let countPossibleElementsInRanges ranges =
    ranges
    |> mergeRanges
    |> Seq.sumBy (fun (min, max) -> max - min + 1I)

let numberOfPossibleElementsInRanges =
    countPossibleElementsInRanges ranges

printfn "Number of possible fresh ingredient IDs: %A" numberOfPossibleElementsInRanges