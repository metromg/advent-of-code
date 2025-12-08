open System.IO

let filePath = "2025/08/input.txt"
let input = File.ReadAllLines filePath

let parseInput (input : string array) =
    input
    |> Array.map (fun row ->
        match row.Split ',' with
        | [| x ; y ; z |] -> bigint.Parse x, bigint.Parse y, bigint.Parse z
        | _ -> failwith "Failed to parse"
    )

let connectPositions maxConnections positions =
    let positionPairsSortedByLeastDistance positions =
        let allUniquePairs sequence =
            sequence
            |> Seq.mapi (fun i a ->
                sequence
                |> Seq.skip (i + 1)
                |> Seq.map (fun b -> a, b))
            |> Seq.concat

        let euclideanDistance (x1 : bigint, y1 : bigint, z1 : bigint) (x2 : bigint, y2 : bigint, z2 : bigint) =
            (x1 - x2) ** 2 + (y1 - y2) ** 2 + (z1 - z2) ** 2 |> double |> sqrt

        positions
        |> allUniquePairs
        |> Seq.sortBy (fun (a, b) -> euclideanDistance a b)

    let connect positionToCircuitMap newCircuitId (a, b) =
        match positionToCircuitMap |> Map.tryFind a, positionToCircuitMap |> Map.tryFind b with
        | Some circuitA, Some circuitB when circuitA <> circuitB ->
            positionToCircuitMap |> Map.map (fun _ value -> if value = circuitA || value = circuitB then newCircuitId else value)
        | Some circuitA, None ->
            positionToCircuitMap |> Map.add b circuitA
        | None, Some circuitB ->
            positionToCircuitMap |> Map.add a circuitB
        | None, None ->
            positionToCircuitMap |> Map.add a newCircuitId |> Map.add b newCircuitId
        | _ ->
            positionToCircuitMap

    positions
    |> positionPairsSortedByLeastDistance
    |> (fun pairs ->
        match maxConnections with
        | Some n -> pairs |> Seq.take n
        | None -> pairs
    )
    |> Seq.indexed
    |> Seq.fold (fun (positionToCircuitMap, lastJoin) (newCircuitId, (a, b)) ->
        let positionToCircuitMap' = connect positionToCircuitMap newCircuitId (a, b)
        let lastJoin' =
            match positionToCircuitMap |> Map.tryFind a, positionToCircuitMap |> Map.tryFind b with
            | Some circuitA, Some circuitB when circuitA = circuitB -> lastJoin
            | _ -> Some (a, b)
        positionToCircuitMap', lastJoin'
    ) (Map.empty, None)

// Part 1
let calculatePart1 positions =
    let positionToCircuitMap, _ = connectPositions (Some 1000) positions

    positionToCircuitMap
    |> Map.toSeq
    |> Seq.groupBy snd
    |> Seq.map (fun (_, positions) -> positions |> Seq.length)
    |> Seq.sortDescending
    |> Seq.take 3
    |> Seq.fold (fun acc size -> acc * size) 1

let part1 = input |> parseInput |> calculatePart1
printfn "Product of three largest circuit sizes of 1000 least distance connections: %i" part1

// Part 2
let calculatePart2 positions =
    let _, lastJoin = connectPositions None positions
    
    match lastJoin with
    | Some ((x1, _, _), (x2, _, _)) -> x1 * x2
    | None -> failwith "No joins"

let part2 = input |> parseInput |> calculatePart2
printfn "Product of X-Coordinates of the two last connections needed to form one big circuit: %A" part2