module Crossword =
    let run =
        let data =
            Data.parseGameFrom "data2.txt"
            |> Option.bind Analysis.findSolutions
            |> Option.map (List.distinct)
            |> Option.map (List.head)
            |> Option.get 
            |> Data.printResults
        System.Console.ReadKey() |> ignore

Crossword.run
