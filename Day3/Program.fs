// Learn more about F# at http://fsharp.org

open System
open System.IO
open System.Text
open System
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
        //printfn "%i" i
        for j=1 to size do
            //printfn "%i" j
            fabric <- NotTaken {XCoord=i;YCoord=j}::fabric
    
    fabric

let tryParseInt i =
    let tryParseIntInner = 
        try 
            i |> int |> Some
        with :? FormatException -> 
            None

    match tryParseIntInner with
    |Some i -> i
    |None -> 0

let toElfRequest (str:string) = 

    let getInt x y= (str.Substring(x,y)) |> tryParseInt 
    let getInt' x = (str.Substring(x)) |> tryParseInt 

    let positionOfAt = str.IndexOf "@"
    let positionOfComma = str.IndexOf ","
    let positionOfColon = str.IndexOf ":"
    let positionOfX = str.IndexOf "x"

    let  number =  getInt 1 positionOfAt

    let  xCoord = getInt positionOfAt positionOfComma

    let yCoord = getInt positionOfComma positionOfColon

    let xSize = getInt positionOfColon positionOfX

    let ySize = getInt' positionOfX

    {Number=number;XCoord=xCoord;YCoord=yCoord;XSize=xSize;YSize=ySize}


[<EntryPoint>]
let main argv =
    let fabric = initFabric 1000
    let inputData = readInputData inputDataPath |> List.map toElfRequest



    printfn "Hello World from F#!"
    0 // return an integer exit code
