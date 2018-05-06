module Analysis

open Types

//let checkIfWordFits word board position =
    
let checkWhereWordFits (board: Board) (occupiedPositions: Map<WordPosition, Word>) (word: Word): WordPosition list =
    board.wordPositions 
    |> List.except (occupiedPositions 
    |> Map.toList 
    |> List.map (fun (k, _) -> k)) 
    |> List.filter (board.wordFitsInPosition word)

let rec findSolutions (game: Game): Game list option =
    //printfn "%s" ("placedWords: " + game.placedWords.Count.ToString() + "; unusedWords: " + game.unusedWords.Length.ToString())

    //if game.unusedWords.Length = 0 then Some [ game ]
    //else
    //    let t =
    //        game.unusedWords
    //        |> List.map (fun word -> word, checkWhereWordFits game.board game.placedWords word) 
    //        |> List.sortBy(fun (word, possiblePositions) -> possiblePositions.Length)
    //        |> List.collect (fun (word, possiblePositions) -> 
    //            possiblePositions 
    //            |> List.map (fun pos -> (game.placeWord word pos) |> Option.bind (fun (game2: Game) -> findSolutions game2))
    //            |> List.choose id
    //            |> List.collect id)
                
    //    if t.Length = 0 then 
    //         Some t 
    //    else printfn "LOL"; None

    //"placedWords: " + game.placedWords.Count.ToString() + "; unusedWords: " + game.unusedWords.Length.ToString())

    if game.unusedWords.Length = 0 then Some [ game ]
    else
        let t =
            game.unusedWords
            |> List.map (fun word -> word, checkWhereWordFits game.board game.placedWords word) 
            |> List.sortBy(fun (word, possiblePositions) -> possiblePositions.Length)
            |> List.collect (fun (word, possiblePositions) -> 
                possiblePositions 
                |> List.map (fun pos -> (game.placeWord word pos) |> Option.bind (findSolutions))
                |> List.choose id
                |> List.collect id)
                
        if t.Length <> 0 then Some t else None

