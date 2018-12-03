// Learn more about F# at http://fsharp.org

open System
open System.IO
open System.Text

type Checksum =
    {Twice:int;
    Thrice:int}

[<Literal>]
let inputDataPath = "./inputData.txt"

let FindDuplicateLetters input =
    let grouped =input |> Seq.groupBy (id) |> Seq.map (fun x -> (fst x, Seq.toList (snd x)|>List.length)) |> Seq.sortBy snd |> Seq.toList

    let doub y=
        if ((List.where (fun x -> snd x = y) grouped).Length > 0)
        then 1
        else 0
    
    {Twice= doub 2; Thrice= doub 3}
        
let readInputData path =
   File.ReadAllLines(path, Encoding.UTF8) |> Array.toList |> List.map FindDuplicateLetters

[<EntryPoint>]
let main argv =
    let checksums = readInputData inputDataPath
    let accumulate = List.fold (fun acc elem -> {Twice= acc.Twice + elem.Twice; Thrice= acc.Thrice + elem.Thrice}) {Twice=0;Thrice=0} checksums
    let checksumResult = accumulate.Thrice * accumulate.Twice

    printfn "Checksum: %i" checksumResult
    0 // return an integer exit code
