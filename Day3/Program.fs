// Learn more about F# at http://fsharp.org

open System
open System.IO
open System.Text
open System
open System
open System.Diagnostics

[<Literal>]
let InputDataPath = "./inputData.txt"
        
let readInputData path =
   File.ReadAllLines(path, Encoding.UTF8) |> Array.toList

type FabricPieceLocation =
    {XCoord:int;
    YCoord:int}

type ElfRequest =
    {Number:int;
    XCoord:int;
    YCoord:int;
    XSize:int;
    YSize:int;
    X2Coord:int;
    Y2Coord:int}

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

    let number =  getInt 0 positionOfAt

    let xCoord = (getInt positionOfAt positionOfComma) + 1

    let yCoord = (getInt positionOfComma positionOfColon) + 1

    let xSize = getInt positionOfColon  positionOfX

    let ySize = getInt positionOfX 0

    {Number=number;XCoord=xCoord;YCoord=yCoord;XSize=xSize;YSize=ySize;X2Coord= xCoord + xSize;Y2Coord=yCoord+ySize}

let processElfRequests (elfRequests:ElfRequest list) (request:ElfRequest) =
    let isNotIntersecting innerRequest=       
        let underOrRight = (innerRequest.XCoord>request.X2Coord || innerRequest.YCoord > request.Y2Coord)
        let overOrLeft = (innerRequest.X2Coord<request.XCoord || innerRequest.Y2Coord < request.YCoord)
        let overAndRight = (innerRequest.XCoord > request.X2Coord && innerRequest.Y2Coord < request.YCoord)
        let underAndRight = (innerRequest.YCoord > request.Y2Coord && innerRequest.X2Coord < request.XCoord)
        underOrRight || overOrLeft || overAndRight || underAndRight

    let calculateIntersectionPlane innerRequest=
        

    let result innerRequest =
        if (innerRequest.Number > request.Number && !isNotIntersecting innerRequest) then


let filterForTaken element =
    match element with
    |NotTaken _ -> false
    |Taken (_,b) -> b.Length > 1

type LoggingBuilder()= 
    member this.Bind(x, f) = 
        let stopWatch = new Stopwatch()
        stopWatch.Start() |> ignore
        printfn "Started action"
        let result = x ()
        stopWatch.Stop() |> ignore
        printfn "Action took: %s" (stopWatch.Elapsed.ToString())
        f result

    member this.Return(x) = 
        x

[<EntryPoint>]
let main argv =
    let logger = LoggingBuilder()
    let loggingWorkflow =
        logger {
            let! fabric = fun () ->(initFabric 1000)
            let! inputData = fun () -> (List.map toElfRequest (readInputData InputDataPath)) 
            let partialElfRequestProcessing =  processElfRequests inputData
            let! resultFabric = fun () -> (List.map partialElfRequestProcessing fabric) 
            let! filteredResult = fun () -> (List.filter filterForTaken resultFabric)
            return filteredResult
        }
   

    printfn "Length of ResultList is: %i" loggingWorkflow.Length
    Console.ReadLine() |> ignore
    0 // return an integer exit code
