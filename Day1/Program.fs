// Learn more about F# at http://fsharp.org

open System
open System.IO
open System.Text

[<Literal>]
let inputDataPath = "./inputData.txt"

let tryParseInt s = 
    try 
        s |> int |> Some
    with :? FormatException -> 
        None

let readInputData path =
   File.ReadAllLines(path, Encoding.UTF8) |> Array.map  tryParseInt 

let ComputeFrequency oldValue newValue=
    let newValueInt =
        match newValue with
        |Some i -> i
        |None -> 0
    
    oldValue + newValueInt

[<EntryPoint>]
let main argv =
    let inputData = readInputData inputDataPath
    inputData |> Array.fold 
    printfn "Hello World from F#!"
    0 // return an integer exit code
