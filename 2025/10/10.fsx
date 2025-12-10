open System.IO

let filePath = "2025/10/input.txt"
let input = File.ReadAllLines filePath

let parseInput (input : string array) =
    input
    |> Array.map (fun row ->
        let elements = row.Split " "
        let targetString = elements.[0]
        let possibleMutationStrings = elements |> Array.filter (fun e -> e.Contains "(")

        let replaceChars replacements input =
            input
            |> Seq.map (fun c -> replacements |> Map.tryFind c |> Option.defaultValue (string c))
            |> String.concat ""

        let binaryToInt s =
            System.Convert.ToInt32(s, 2)

        let target =
            targetString
            |> replaceChars (Map.ofList [
                '[', ""
                ']', ""
                '.', "0"
                '#', "1"
            ])
            |> binaryToInt

        let possibleMutations =
            possibleMutationStrings
            |> Array.map (fun possibleMutationString ->
                let fixedLength = targetString.Length - 2
                let binaryOneDigits =
                    possibleMutationString
                    |> replaceChars (Map.ofList [ '(', "" ; ')', ""])
                    |> fun v -> v.Split ","
                    |> Array.map int
                let binaryNumberString =
                    [0 .. fixedLength - 1]
                    |> List.map (fun i -> if binaryOneDigits |> Array.contains i then "1" else "0")
                    |> String.concat ""
                binaryNumberString |> binaryToInt
            )
        target, possibleMutations
    )

// Part 1 (No solution to part 2, it kills me)
let searchNumberOfLeastMutations target possibleMutations =
    let rec search queue visited =
        match queue with
        | [] -> None
        | current :: rest ->
            let queue', visited', found =
                possibleMutations
                |> Seq.fold (fun (q, v, f) m ->
                    match f with
                    | Some _ -> q, v, f
                    | None ->
                        let next = current ^^^ m
                        if v |> Map.containsKey next then
                            q, v, None
                        else
                            let v' = v |> Map.add next (current, m)
                            if next = target then
                                q, v', Some v'
                            else
                                q @ [next], v', None
                ) (rest, visited, None)

            match found with
            | Some v -> Some v
            | None -> search queue' visited'

    let start = 0
    
    match search [start] Map.empty with
    | Some visited ->
        let usedMutations =
            Seq.unfold (fun current ->
                if current = start then
                    None
                else
                    let previous, used = visited |> Map.find current
                    Some (used, previous)
            ) target
            |> Seq.toList
            |> Seq.rev
        usedMutations |> Seq.length
    | None -> 0

let numberOfLeastPossibleMutations =
    input
    |> parseInput
    |> Seq.map (fun (target, possibleMutations) -> searchNumberOfLeastMutations target possibleMutations)
    |> Seq.sum

printfn "Number of least possible button presses: %i" numberOfLeastPossibleMutations