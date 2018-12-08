// Learn more about F# at http://fsharp.org

open System
open System.IO
open System.Text

[<Literal>]
let InputDataPath = "./inputData.txt"
        
let readInputData path =
   File.ReadAllLines(path, Encoding.UTF8) |> Array.toList

let concatListTimes inputList times =
   let mutable resultList = [] 
   
   for i=1 to times do
      resultList <- List.append resultList inputList

   resultList

let getDifference (x:string,y:string) =
   (List.fold2 (fun acc elem1 elem2 -> if (elem1 = elem2) then acc else acc + 1) 0 (x.ToCharArray()|>Array.toList) (y.ToCharArray()|>Array.toList)) = 1

let getDistinct (x:string,y:string) =
   let mutable accumulator = ""
   let updateAccumulator (value:char) =
      let appendChar = Array.append (accumulator.ToCharArray()) [|value|]
      accumulator <-String.Concat appendChar
      accumulator

   List.fold2 (fun acc (elem1:char) (elem2:char) -> if (elem1 = elem2) then (updateAccumulator elem1) else accumulator) accumulator (x.ToCharArray()|>Array.toList) (y.ToCharArray()|>Array.toList)

[<EntryPoint>]
let main argv =
    let checksums = readInputData InputDataPath
    let checksumListLength = checksums.Length
    let duplicateValues = List.collect (fun x -> List.replicate checksumListLength x) checksums
    let duplicateList = concatListTimes checksums checksumListLength
    let zippedList = List.zip duplicateValues duplicateList
    let result = List.filter getDifference zippedList |> List.map getDistinct
    List.iter (fun x -> printfn "result: %s;" x ) result 
    //let accumulate = List.
    //let checksumResult = accumulate.Thrice * accumulate.Twice
    
    //printfn "Checksum: %i" checksumResult
    0 // return an integer exit code
