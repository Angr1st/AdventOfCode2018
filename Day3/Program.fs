// Learn more about F# at http://fsharp.org

open System
open System.IO
open System.Text
open System
open System

[<Literal>]
let InputDataPath = "./inputData.txt"
        
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
    |Taken of FabricPieceLocation * ElfRequest list

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
    let dif x y = y - x
    let getInt x y =
        let getInt' x = (str.Substring(x)) |> tryParseInt 
        let getInt'' x y = (str.Substring( x, (dif x y) )) |> tryParseInt 
        let innerX = x + 1 
        if (y = 0) then 
            (getInt' innerX) 
        else 
            (getInt'' innerX y)

    let positionOfAt = str.IndexOf "@"
    let positionOfComma = str.IndexOf ","
    let positionOfColon = str.IndexOf ":"
    let positionOfX = str.IndexOf "x"

    let  number =  getInt 0 positionOfAt

    let  xCoord = (getInt positionOfAt positionOfComma) + 1

    let yCoord = (getInt positionOfComma positionOfColon) + 1

    let xSize = getInt positionOfColon  positionOfX

    let ySize = getInt positionOfX 0

    {Number=number;XCoord=xCoord;YCoord=yCoord;XSize=xSize;YSize=ySize}

let processElfRequests (elfRequests:ElfRequest list) (leFabricPiece:FabricPiece) =
    let findMatchingElfRequest x y (request:ElfRequest) =
        let mutable resultList = []
        for i = request.XCoord to request.XSize + request.XCoord do
            for j = request.YCoord to request.YSize + request.YCoord do
                resultList <- {XCoord=i;YCoord=j}::resultList

        (List.where (fun elem -> elem.XCoord = x && elem.YCoord = y) resultList).Length = 1    

    let getElfRequests x y = List.filter (findMatchingElfRequest x y) elfRequests
    
    let listEmpty (someList:ElfRequest list) = someList.Length = 0

    let getFabricLocation =
        match leFabricPiece with
        |NotTaken a -> a
        |Taken (a,_) -> a

    let selectedElfRequests = getElfRequests getFabricLocation.XCoord getFabricLocation.YCoord

    if (selectedElfRequests |> listEmpty) then 
        leFabricPiece
    else
        Taken ({XCoord=getFabricLocation.XCoord;YCoord=getFabricLocation.YCoord},selectedElfRequests)

let filterForTaken element =
    match element with
    |NotTaken _ -> false
    |Taken _ -> true

[<EntryPoint>]
let main argv =
    let fabric = initFabric 1000
    let inputData = readInputData InputDataPath |> List.map toElfRequest
    let partialElfRequestProcessing = processElfRequests inputData
    let resultFabric = fabric |> List.map partialElfRequestProcessing |> List.filter filterForTaken


    printfn "Length of ResultList is: %i" resultFabric.Length
    0 // return an integer exit code
