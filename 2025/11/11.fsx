open System
open System.IO

let filePath = "2025/11/input.txt"
let input = File.ReadAllLines filePath

let parseInput (input : string array) =
    input
    |> Seq.fold (fun graph row ->
        let addConnection source target graph =
            graph |> Map.change source (function Some e -> Some (e |> Set.add target) | None -> Some (Set.empty |> Set.add target))

        let split = row.Split(":", StringSplitOptions.RemoveEmptyEntries)
        match split with
        | [| node ; outputs |] ->
            outputs.Split(" ", StringSplitOptions.RemoveEmptyEntries)
            |> Array.fold (fun g o -> g |> addConnection node o) graph
        | _ -> failwith "Failed to parse"
    ) Map.empty

let countDistinctPaths start required goal graph =
    let rec traverse node remaining cache =
        let cacheKey = node, remaining

        match cache |> Map.tryFind cacheKey with
        | Some result ->
            result, cache
        | None ->
            let remaining' = remaining |> Set.remove node

            let result, cache' =
                if node = goal then
                    if remaining' |> Set.isEmpty then 1I, cache else 0I, cache
                else
                    graph
                    |> Map.tryFind node
                    |> Option.defaultValue Set.empty
                    |> Seq.fold (fun (acc, c) n ->
                        let r, c' = traverse n remaining' c
                        acc + r, c'
                    ) (0I, cache)

            result, cache' |> Map.add cacheKey result

    traverse start (required |> Set.ofList) Map.empty |> fst

// Part 1
let numberOfDistinctPathsYouToOut =
    input
    |> parseInput
    |> countDistinctPaths "you" [] "out"

printfn "Number of distinct paths you to out: %A" numberOfDistinctPathsYouToOut

// Part 2
let numberOfDistinctPathsSvrToOutPassingDacFft =
    input
    |> parseInput
    |> countDistinctPaths "svr" [ "dac" ; "fft" ] "out"

printfn "Number of distinct paths svr to out passing through dac and fft: %A" numberOfDistinctPathsSvrToOutPassingDacFft