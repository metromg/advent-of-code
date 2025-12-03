open System.IO
open System.Numerics

let filePath = "2025/03/input.txt"
let input = File.ReadAllLines filePath

let parseInput (input : string array) =
    input |> Array.map (fun bank -> bank |> Seq.map string |> Array.ofSeq |> Array.map int)

let findMaximumJoltage n (bank : int array) =
    let rec pick needed startIndex acc =
        if needed = 0 then
            acc |> List.rev
        else
            let endIndex = bank.Length - needed
            let bestIndex, bestDigit =
                [ startIndex .. endIndex ]
                |> List.fold (fun (bestIdx, bestDigit) i ->
                    let digit = bank.[i]
                    if digit > bestDigit then
                        i, digit
                    else
                        bestIdx, bestDigit
                ) (startIndex, bank.[startIndex])

            pick (needed - 1) (bestIndex + 1) (bestDigit :: acc)

    pick n 0 []
    |> List.map string
    |> String.concat ""
    |> BigInteger.Parse

// Part 1
let sumOfMaximumJoltagesTwoDigits =
    input
    |> parseInput
    |> Array.map (findMaximumJoltage 2)
    |> Array.sum

printfn "Sum of maximum joltages with two digits is %A" sumOfMaximumJoltagesTwoDigits

// Part 2
let sumOfMaximumJoltagesTwelveDigits =
    input
    |> parseInput
    |> Array.map (findMaximumJoltage 12)
    |> Array.sum

printfn "Sum of maximum joltages with twelve digits is %A" sumOfMaximumJoltagesTwelveDigits