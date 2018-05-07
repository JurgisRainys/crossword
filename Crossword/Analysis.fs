module Analysis

open Types

let checkWhereWordFits (board: Board) (occupiedPositions: Map<WordPosition, Word>) (word: Word): WordPosition list =
    board.wordPositions 
    |> List.except (occupiedPositions 
    |> Map.toList 
    |> List.map (fun (k, _) -> k)) 
    |> List.filter (board.wordFitsInPosition word)

let rec findSolutions (game: Game): Game list =
    if game.unusedWords.Length = 0 then [ game ]
    else 
        let solutions =
            game.unusedWords
            |> List.map (fun word -> word, checkWhereWordFits game.board game.placedWords word) 
            |> List.sortBy(fun (word, possiblePositions) -> possiblePositions.Length)
            |> List.head
            |> (fun (word, possiblePositions) -> 
                possiblePositions 
                |> List.collect (fun pos -> 
                    (game.placeWord word pos) 
                    |> Option.map (findSolutions) 
                    |> Option.defaultValue []
                ))
        solutions
