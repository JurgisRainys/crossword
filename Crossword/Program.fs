module Crossword =
    let run =
        let data =
            Data.parseGameFrom "data.txt"
            |> Option.map Analysis.findSolutions
        System.Console.ReadKey() |> ignore

Crossword.run
