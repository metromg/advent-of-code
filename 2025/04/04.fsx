open System.IO

let filePath = "2025/04/input.txt"
let input = File.ReadAllLines filePath

let parseInput (input : string array) =
    input
    |> Array.mapi (fun rowIndex row ->
        row.ToCharArray()
        |> Array.indexed
        |> Array.filter (fun (_, field) -> field = '@')
        |> Array.map (fun (colIndex, _) -> rowIndex, colIndex)
    )
    |> Array.concat
    |> Set.ofArray

let accessiblePositions positions =
    let countNeighbors positions (row, col) =
        let neighborPositions =
            [
                row - 1, col - 1 ;
                row - 1, col ;
                row - 1, col + 1 ;
                row, col - 1 ;
                row, col + 1 ;
                row + 1, col - 1 ;
                row + 1, col ;
                row + 1, col + 1 ;
            ]

        neighborPositions
        |> Seq.filter (fun neighborPos -> positions |> Set.contains neighborPos)
        |> Seq.length

    let isAccessible positions pos =
        let inaccessibleThreshold = 4
        let neighborCount = countNeighbors positions pos
        neighborCount < inaccessibleThreshold

    positions |> Seq.filter (isAccessible positions) |> Set.ofSeq

let positions = parseInput input

// Part 1
let numberOfAccessiblePositions = positions |> accessiblePositions |> Set.count
printfn "Number of accessible paper rolls: %i" numberOfAccessiblePositions

// Part 2
let rec removeAccessible positions totalRemovedCount =
    let toRemove = positions |> accessiblePositions

    if toRemove |> Set.isEmpty then
        totalRemovedCount
    else
        let remaining = Set.difference positions toRemove
        let removedCount = toRemove |> Set.count
        removeAccessible remaining (totalRemovedCount + removedCount)

let numberOfAccessiblePositionsAfterMultipleRemovePasses = removeAccessible positions 0
printfn "Number of accessible paper rolls after multiple remove passes: %i" numberOfAccessiblePositionsAfterMultipleRemovePasses