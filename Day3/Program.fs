// Learn more about F# at http://fsharp.org

open System
open System.IO
open System.Text
open System

[<Literal>]
let inputDataPath = "./inputData.txt"
        
let readInputData path =
   File.ReadAllLines(path, Encoding.UTF8) |> Array.toList

type ElfRequest =
    {Number:int;
    XCoord:int;
    YCoord:int;
    XSize:int;
    YSize:int}

type FabricPieceLocation =
    {XCoord:int;
    YCoord:int}

type FabricPiece =
    |NotTaken of FabricPieceLocation
    |Taken of FabricPieceLocation * Memory<ElfRequest>[]

let initFabric size =
    let mutable fabric = List.empty<FabricPiece>

    for i=1 to size do
        for j=1 to size do
            fabric <- NotTaken {XCoord=i;YCoord=j}::fabric
    
    fabric

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
