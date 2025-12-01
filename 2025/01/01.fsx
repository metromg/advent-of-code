open System.IO

let filePath = "2025/01/input.txt"
let input = File.ReadAllLines(filePath)

let parseInput inputRows =
    let (|StartsWith|_|) (c: char) (s: string) =
        if not (isNull s) && s.Length > 0 && s.[0] = c
        then Some (s.Substring 1)
        else None

    inputRows |> Seq.map (fun value ->
        match value with
        | StartsWith 'L' rest -> int rest * -1
        | StartsWith 'R' rest -> int rest
        | _ -> failwith "failed to parse"
    )

// Part 1
let calculateNumberOfZeroPointers inputNumbers =
    inputNumbers
    |> Seq.scan (fun acc delta -> (acc + delta) % 100) 50
    |> Seq.filter (fun state -> state = 0)
    |> Seq.length

let numberOfZeroPointers =
    input
    |> parseInput
    |> calculateNumberOfZeroPointers

printfn "The dial pointed at 0 a total of %i times" numberOfZeroPointers

// Part 2
let calculateNumberOfZeroPasses inputNumbers =
    let quantizedInputNumbers =
        inputNumbers
        |> Seq.collect (fun value ->
            Seq.init (abs value) (fun _ -> sign value)
        )

    quantizedInputNumbers |> calculateNumberOfZeroPointers

let numberOfZeroPasses =
    input
    |> parseInput
    |> calculateNumberOfZeroPasses

printfn "The dial passed through 0 a total of %i times" numberOfZeroPasses