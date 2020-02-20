module HashCode2020

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO

type Book = int

type Library = {
    Id: int
    Books: Book Set
    SignupTime: int
    ScanningRate: int
}

type ProblemStatement = {
    Libraries: Library list
    BookScores: int[]
    Deadline: int
}
with
    member x.GetBookScore (book: Book) = x.BookScores.[book]

let parseInput lines =
    let parseLine (x: string) = x.Split(' ') |> Array.map int
    let header :: lScores :: xs = lines
    let [|bookCount; libraryCount; deadline|] = parseLine header
    let scores = parseLine lScores
    let libraries =
        xs
        |> List.chunkBySize 2
        |> List.mapi (fun i [libInfo; books] ->
            let [|_; signupTime; scanningRate|] = parseLine libInfo
            {
                Id = i
                Books = set <| parseLine books
                SignupTime = signupTime
                ScanningRate = scanningRate
            })
    {
        Libraries = libraries
        BookScores = scores
        Deadline = deadline
    }

type ProblemSubmission = ProblemSubmission of (int * Book list) list

let solveIt statement () =
    let libraries =
        statement.Libraries
        |> List.sortByDescending (fun lib ->
            let totalScore = Seq.sumBy statement.GetBookScore lib.Books |> float
            Math.Cbrt(float lib.ScanningRate * totalScore / float lib.SignupTime))

    let scannedBooks = HashSet()

    libraries
    |> List.map (fun lib ->
        let books =
            lib.Books
            |> Seq.where (scannedBooks.Contains >> not)
            |> Seq.sortByDescending statement.GetBookScore
            |> List.ofSeq
        Seq.iter (scannedBooks.Add >> ignore) books
        lib.Id, books)
    |> ProblemSubmission

let renderSubmission (ProblemSubmission sub) =
    sub
    |> List.collect (fun (id, books) ->
        [
            sprintf "%d %d" id books.Length
            books |> Seq.map string |> String.concat " "
        ]
    )
    |> (fun xs -> string sub.Length :: xs)
    |> String.concat "\n"

let timeIt label f =
    let sw = Stopwatch()
    sw.Start()
    let x = f()
    sw.Stop()
    printfn "Time for %s: %O" label sw.Elapsed
    x

Environment.CurrentDirectory
|> Directory.EnumerateFiles
|> Seq.filter (Path.GetExtension >> ((=) ".txt"))
|> Seq.map (Path.GetFileName)
|> Seq.iter (fun x ->
    let data = File.ReadLines x |> List.ofSeq
    let outputFileName = Path.ChangeExtension(x, ".out")
    let problemStatement = parseInput data
    let theHolySolution = solveIt problemStatement |> timeIt x |> renderSubmission
    File.WriteAllText(outputFileName, theHolySolution))
