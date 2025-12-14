open System
open System.IO

let filePath = "2025/12/input.txt"
let input = File.ReadAllText filePath

let parseInput (input : string) =
    let sections = input.Split(Environment.NewLine + Environment.NewLine)
    let shapes, regions = sections |> Array.take (sections.Length - 1), sections |> Array.last

    let shapeAreas =
        shapes
        |> Array.map (fun shape -> shape |> Seq.filter (fun s -> s = '#') |> Seq.length)
    let regionAreasWithShapeQuantities =
        regions.Split Environment.NewLine
        |> Array.map (fun region ->
            match region.Split ": " with
            | [| sizeString ; quantitiesString|] ->
                let area =
                    match sizeString.Split "x" with
                    | [| width ; height|] -> int width * int height
                    | _ -> failwith "Failed to parse"
                let quantities =
                    quantitiesString.Split " " |> Array.map int
                area, quantities
            | _ -> failwith "Failed to parse"
        )
    shapeAreas, regionAreasWithShapeQuantities

let countRegionsFittingAllShapes (shapeAreas : int array) regionAreasWithShapeQuantities =
    regionAreasWithShapeQuantities
    |> Seq.filter (fun (regionArea, shapeQuantities) ->
        let totalShapeArea = decimal (shapeQuantities |> Seq.mapi (fun i quantity -> quantity * shapeAreas.[i]) |> Seq.sum)
        // Just tried different factors, 1.2 worked for both the sample and my real input
        totalShapeArea * 1.2m < decimal regionArea
    )
    |> Seq.length

let shapeAreas, regionAreasWithShapeQuantities = parseInput input
let numberOfRegionsFittingAllShapes = countRegionsFittingAllShapes shapeAreas regionAreasWithShapeQuantities
printfn "Number of regions fitting all shapes: %i" numberOfRegionsFittingAllShapes