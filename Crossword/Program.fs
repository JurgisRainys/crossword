module Crossword =
    open Crossword
    open Helpers
    open Data

    let run =
        printfn "%A" (isAllowedWordSymbol 'Į')
        let data = readData "data.txt"
        System.Console.ReadKey() |> ignore

Crossword.run
