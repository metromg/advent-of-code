open System.IO

let filePath = "2025/07/input.txt"
let input = File.ReadAllLines filePath

let parseInput (input : string array) =
    let startColIndex =
        input.[0].ToCharArray()
        |> Array.findIndex (fun v -> v = 'S')
    let splitters =
        input
        |> Array.mapi (fun rowIndex row ->
            row.ToCharArray()
            |> Array.indexed
            |> Array.filter (fun (_, v) -> v = '^')
            |> Array.map (fun (colIndex, _) -> rowIndex, colIndex)
        )
        |> Array.concat
        |> Set.ofArray
    (0, startColIndex), splitters

let startingPos, splitters = parseInput input

// Part 1
let countSplits startingPos splitters =
    let maxRow = splitters |> Set.maxElement |> fst

    let rec count (row, col) visited =
        if row > maxRow || visited |> Set.contains (row, col) then
            0, visited
        elif splitters |> Set.contains (row, col) then
            let visited' = visited |> Set.add (row, col)
            let left, visited'' = count (row, col - 1) visited'
            let right, visited''' = count (row, col + 1) visited''
            left + right + 1, visited'''
        else
            count (row + 1, col) (visited |> Set.add (row, col))

    let totalCount, _ = count startingPos Set.empty
    totalCount

let numberOfSplits = countSplits startingPos splitters
printfn "Number of splits: %i" numberOfSplits

// Part 2
let countTimelines startingPos splitters =
    let maxRow = splitters |> Set.maxElement |> fst

    let rec count (row, col) cache =
        match cache |> Map.tryFind (row, col) with
        | Some value ->
            value, cache
        | None ->
            let result, newCache =
                if row > maxRow then
                    1I, cache
                elif splitters |> Set.contains (row, col) then
                    let left, cache' = count (row, col - 1) cache
                    let right, cache'' = count (row, col + 1) cache'
                    left + right, cache''
                else
                    count (row + 1, col) cache
            result, newCache |> Map.add (row, col) result
    
    let totalCount, _ = count startingPos Map.empty
    totalCount

let numberOfTimelines = countTimelines startingPos splitters
printfn "Number of timelines: %A" numberOfTimelines