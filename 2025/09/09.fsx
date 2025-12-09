open System.IO

let filePath = "2025/09/input.txt"
let input = File.ReadAllLines filePath

let parseInput (input : string array) =
    input
    |> Array.map (fun row ->
        match row.Split ',' with
        | [| x ; y |] -> bigint.Parse x, bigint.Parse y
        | _ -> failwith "Failed to parse"
    )

let allUniquePairs sequence =
    sequence
    |> Seq.mapi (fun i a ->
        sequence
        |> Seq.skip (i + 1)
        |> Seq.map (fun b -> a, b))
    |> Seq.concat

let maxArea cornerPairs =
    cornerPairs
    |> Seq.map (fun ((x1, y1), (x2, y2)) -> (abs (x2 - x1) + 1I) * (abs (y2 - y1) + 1I))
    |> Seq.max    

// Part 1
let maxAreaOfRectangle =
    input
    |> parseInput
    |> allUniquePairs
    |> maxArea

printfn "Max area of rectangle: %A" maxAreaOfRectangle

// Part 2
let calculateMaxAreaOfRectangleFullyInsideBoundary positions =
    let edges =
        Seq.append positions [positions |> Seq.rev |> Seq.last]
        |> Seq.pairwise

    let isAnyEdgeInsideRectangle (minX, maxX, minY, maxY) =
        edges
        |> Seq.exists (fun ((x1, y1), (x2, y2)) ->
            let maxEdgeX = max x1 x2
            let minEdgeX = min x1 x2
            let maxEdgeY = max y1 y2
            let minEdgeY = min y1 y2
            minX < maxEdgeX && minEdgeX < maxX && minY < maxEdgeY && minEdgeY < maxY
        )

    positions
    |> allUniquePairs
    |> Seq.filter (fun ((x1, y1), (x2, y2)) ->
        let minX = min x1 x2
        let maxX = max x1 x2
        let minY = min y1 y2
        let maxY = max y1 y2
        not (isAnyEdgeInsideRectangle (minX, maxX, minY, maxY))
    )
    |> maxArea

let maxAreaOfRectangleFullyInsideBoundary =
    input
    |> parseInput
    |> calculateMaxAreaOfRectangleFullyInsideBoundary

printfn "Max area of rectangle fully inside boundary: %A" maxAreaOfRectangleFullyInsideBoundary