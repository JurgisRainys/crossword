module Crossword =
    let run =
        let data =
            Data.parseGameFrom "data.txt"
            |> Option.map Analysis.findSolutions
            |> Option.map (List.distinctBy (fun t -> t.board.cells))
            |> Option.map (List.map (Data.printResults)) 
        System.Console.ReadKey() |> ignore

Crossword.run
