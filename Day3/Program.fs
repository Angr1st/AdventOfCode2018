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

type FabricPieceRequest =
    {Number:int;
    XCoord:int;
    YCoord:int;
    XY:string}

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

let listAppend one two =
    List.append one two

let processElfRequests (elfRequests:ElfRequest list) (request:ElfRequest) =
    let createCoordList x y xSize ySize =
            let mutable resultList = []
            for i = x to xSize + x do
                for j = y to ySize + y do
                    resultList <- {XCoord=i;YCoord=j}::resultList
            resultList
    
    let requestCoords = createCoordList request.XCoord request.YCoord request.XSize request.YSize

    let isNotIntersecting innerRequest=       
        let underOrRight = (innerRequest.XCoord>request.X2Coord || innerRequest.YCoord > request.Y2Coord)
        let overOrLeft = (innerRequest.X2Coord<request.XCoord || innerRequest.Y2Coord < request.YCoord)
        let overAndRight = (innerRequest.XCoord > request.X2Coord && innerRequest.Y2Coord < request.YCoord)
        let underAndRight = (innerRequest.YCoord > request.Y2Coord && innerRequest.X2Coord < request.XCoord)
        underOrRight || overOrLeft || overAndRight || underAndRight

    let calculateIntersectionPlane innerRequest=
        let innerRequestCoords = createCoordList innerRequest.XCoord innerRequest.YCoord innerRequest.XSize innerRequest.YSize

        let tryFindPredicate (elem:FabricPieceLocation) =
            let tryFindResult = innerRequestCoords |> List.tryFind (fun innerElem -> elem.XCoord = innerElem.XCoord && elem.YCoord = innerElem.XCoord )

            match tryFindResult with
            |Some x -> true
            |None -> false

        let foldToIntersection acc elem =
            let intersects = tryFindPredicate elem

            let fabRequest = {Number = innerRequest.Number; XCoord = elem.XCoord; YCoord = elem.YCoord; XY = elem.XCoord.ToString() + elem.YCoord.ToString()}

            if intersects then fabRequest::acc
            else acc


        requestCoords |> List.fold foldToIntersection []
        

    let result innerRequest =
        if (innerRequest.Number > request.Number && not(isNotIntersecting innerRequest)) then
            calculateIntersectionPlane innerRequest
        else []

    elfRequests |> List.map result |> List.reduce listAppend

let transformTheGroups (group:string * FabricPieceRequest list) =
    let fabReqList = snd group
    let IsLongerThanOne = fabReqList.Length > 1

    if IsLongerThanOne then 1 else 0
   

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
            let! inputData = fun () -> (List.map toElfRequest (readInputData InputDataPath)) 
            let partialElfRequestProcessing =  processElfRequests inputData
            let! resultsList = fun () -> (List.map partialElfRequestProcessing inputData) 
            let! preliminaryResult = fun () -> (List.reduce listAppend resultsList)
            let preResultLength = preliminaryResult.Length
            let! result = fun () -> ((preliminaryResult |> List.groupBy (fun index -> index.XY)) |> List.map transformTheGroups |> List.reduce (fun one two -> one + two) )
            return result
        }
   

    printfn "Length of ResultList is: %i" loggingWorkflow
    Console.ReadLine() |> ignore
    0 // return an integer exit code
