// Learn more about F# at http://fsharp.org

open System
open System.IO
open System.Text
open System.Collections.Generic

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

type Accumulator=
    {List:int list;
    Value:int;
    FirstDoubleFrequency:int option}

let accumulate acc element =
    let resultValue =
        acc.Value + element
    
    let newList =
        resultValue::acc.List
    
    let doubleFrequency =
        try
            List.find (fun x -> x = resultValue) acc.List |> Some
        with :? KeyNotFoundException ->
            None

    let firstDoubleWins =
        match acc.FirstDoubleFrequency with
            |None -> doubleFrequency
            |Some i -> Some i

    {List=newList;Value=resultValue;FirstDoubleFrequency=firstDoubleWins}

let turnToText acc =
    match acc.FirstDoubleFrequency with
    |Some i -> String.Format("{0}", i)
    |None -> "Nothing"

[<EntryPoint>]
let main argv =
    let inputData = readInputData inputDataPath
    let preparedInputData = 0::inputData
    let result = List.fold accumulate {List=[];Value = 0; FirstDoubleFrequency=None} preparedInputData 
    let textResult = turnToText result
    printfn "Result Frequency is: %i; First duplicate Frequency is: %s" result.Value textResult
    let orderedList = List.sort result.List
    orderedList |> List.iter (printfn "%i")
    0 // return an integer exit code
