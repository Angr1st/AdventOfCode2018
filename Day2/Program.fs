// Learn more about F# at http://fsharp.org

open System
open System.IO
open System.Text

[<Literal>]
let inputDataPath = "./inputData.txt"
        
let readInputData path =
   File.ReadAllLines(path, Encoding.UTF8) |> Array.toList

let concatListTimes inputList times =
   let mutable resultList = [] 
   
   for i=1 to times do
      resultList <- inputList::resultList

   resultList

[<EntryPoint>]
let main argv =
    let checksums = readInputData inputDataPath
    let checksumListLength = checksums.Length
    let duplicateValues = List.replicate checksumListLength checksums
    //let accumulate = List.
    //let checksumResult = accumulate.Thrice * accumulate.Twice
    
    //printfn "Checksum: %i" checksumResult
    0 // return an integer exit code
