open System
open System.IO
open System.Numerics

let filePath = "2025/06/input.txt"
let input = File.ReadAllLines filePath

type Operator = Addition | Multiplication

let parseInput (input : string array) =
    let chunkIntoSizes sizes (rowChars : string array) =
        sizes
        |> Seq.fold (fun (currentIndex, result) size ->
            let chunk = rowChars.[ currentIndex .. currentIndex + size - 1 ] |> String.concat ""
            currentIndex + size + 1, chunk :: result
        ) (0, [])
        |> snd
        |> List.rev
        
    let rowsAsChars = input |> Seq.map (fun row -> row |> Seq.map string |> Array.ofSeq)

    let columnLengths =
        rowsAsChars
        |> Seq.find (fun rowChars -> rowChars |> Array.contains "+" || rowChars |> Array.contains "*")
        |> Seq.fold (fun lengths char ->
            match lengths, char with
            | [], _ -> [1]
            | _, "+" | _, "*" -> 1 :: List.head lengths - 1 :: List.tail lengths
            | _, " " -> List.head lengths + 1 :: List.tail lengths
            | _ -> failwith "Unexpected character in operator line"
        ) []
        |> List.rev

    rowsAsChars
    |> Seq.map (chunkIntoSizes columnLengths)
    |> Seq.transpose
    |> Seq.map (fun col ->
        let operator = (col |> Seq.last).Trim()
        let numberStringsWithPadding = col |> Seq.take ((col |> Seq.length) - 1)

        match operator with
        | "+" -> Addition, numberStringsWithPadding
        | "*" -> Multiplication, numberStringsWithPadding
        | _ -> failwith "Failed to parse operator"
    )

let calculate calculation =
    match calculation with
    | Addition, numberStrings -> numberStrings |> Seq.map BigInteger.Parse |> Seq.sum
    | Multiplication, numberStrings -> numberStrings |> Seq.map BigInteger.Parse |> Seq.fold (fun acc num -> acc * num) 1I

// Part 1
let sumOfCalculationsLeftToRight =
    input
    |> parseInput
    |> Seq.map calculate
    |> Seq.sum

printfn "Sum of calculations left to right is %A" sumOfCalculationsLeftToRight

// Part 2
let transposeNumberStrings numberStringsWithPadding =
    numberStringsWithPadding
    |> Seq.map (fun numberString -> numberString |> Seq.map string |> Array.ofSeq)
    |> Seq.transpose
    |> Seq.map (fun numberChars -> numberChars |> String.concat "")

let sumOfCalculationsTopToBottom =
    input
    |> parseInput
    |> Seq.map (fun (operator, numberStrings) -> operator, transposeNumberStrings numberStrings)
    |> Seq.map calculate
    |> Seq.sum

printfn "Sum of calculations top to bottom is %A" sumOfCalculationsTopToBottom