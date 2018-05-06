module Analysis

open Types

//let checkIfWordFits word board position =
    

let checkWhereWordFits (board: Board) (occupiedPositions: Map<WordPosition, Word>) (word: Word): WordPosition list = List.empty
    //board.wordPositions |> List.filter (occupiedPositions)
    

let rec findSolutions (game: Game) =
    let x = 
        game.unusedWords.Head 
        |> checkWhereWordFits game.board game.placedWords
        |> List.map (game.placeFirstUnusedWord)
            
    let word = game.unusedWords.[26]
    let pos = game.board.wordPositions.[53]
    let xxx = game.placeWord word pos
    //        |> List.map (fun word -> word, checkWhereWordFits game.board game.placedWords word) 
    //        |> List.collect (fun (word, possiblePositions) -> 
    //            possiblePositions |> List.map (fun pos -> game.placeFirstUnusedWord pos |> Option.map findSolutions))
    //        |> List.choose id
    //x

    //let x = game.unusedWords 
    //        |> List.map (fun word -> word, checkWhereWordFits game.board game.placedWords word) 
    //        |> List.collect (fun (word, possiblePositions) -> 
    //            possiblePositions |> List.map (fun pos -> game.placeWord word pos |> Option.map findSolutions))
    //        |> List.choose id
    //x

    printfn "%A" game.board.wordPositions

