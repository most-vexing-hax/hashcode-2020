module HashCode2020

open System
open System.Diagnostics
open System.IO

type Book = int

type Library = {
    Books: Book Set
    SignupTime: int
    ScanningRate: int
}

type ProblemStatement = {
    Libraries: Library list
    BookScores: int[]
    Deadline: int
}

let parseInput lines =
    let parseLine (x: string) = x.Split(' ') |> Array.map int
    let header :: lScores :: xs = lines
    let [|bookCount; libraryCount; deadline|] = parseLine header
    let scores = parseLine lScores
    let libraries =
        xs
        |> List.chunkBySize 2
        |> List.map (fun [libInfo; books] -> 
            let [|_; signupTime; scanningRate|] = parseLine libInfo
            {
                Books = set <| parseLine books
                SignupTime = signupTime
                ScanningRate = scanningRate
            })
    {
        Libraries = libraries
        BookScores = scores
        Deadline = deadline
    }

let timeIt label f =
    let sw = Stopwatch()
    sw.Start()
    let x = f()
    sw.Stop()
    printfn "Time for %s: %O" label sw.Elapsed
    x

let solveIt statement () = [| |]

Environment.CurrentDirectory
|> Directory.EnumerateFiles
|> Seq.filter (Path.GetExtension >> ((=) ".txt"))
|> Seq.map (Path.GetFileName)
|> Seq.iter (fun x ->
    let data = File.ReadLines x |> List.ofSeq
    let outputFileName = Path.ChangeExtension(x, ".out")
    let problemStatement = parseInput data
    let theHolySolution = solveIt problemStatement |> timeIt x
    File.WriteAllLines(outputFileName, theHolySolution))
