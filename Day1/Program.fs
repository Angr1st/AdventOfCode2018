// Learn more about F# at http://fsharp.org

open System
open System.IO
open System.Text

[<Literal>]
let inputDataPath = "./inputData.txt"

let tryParseInt i =
    let tryParseIntInner = 
        try 
            i |> int |> Some
        with :? FormatException -> 
            None

    match tryParseIntInner with
    |Some i -> i
    |None -> 0

let readInputData path =
   File.ReadAllLines(path, Encoding.UTF8) |> Array.map  tryParseInt |> Array.toList

[<EntryPoint>]
let main argv =
    let inputData = readInputData inputDataPath
    let preparedInputData = 0::inputData
    let result = preparedInputData |> List.sum
    printfn "Result Frequency is: %i" result
    0 // return an integer exit code
