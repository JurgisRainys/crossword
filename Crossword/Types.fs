module Types

open Helpers

type SType = Vowel | Consonant
type Letter = private { ``type``: SType; ch: char} with
    static member create (ch: char) = 
        //(SType.get ch) |> Option.bind (fun ``type`` -> if ``type`` = Space then None else Some ``type``)
        if (isVowel ch) then Some { ``type`` = Vowel; ch = ch }
        else if (isConsonant ch) then Some { ``type`` = Consonant; ch = ch }
        else None

type Word = Letter list

let getWordLetterTypes word = word |> List.map (fun { ch = _; ``type`` = letterType } -> letterType)

type Coordinate = { x: int; y: int }
type Cell = {``type``: SType; occupant: char option}
type PositionedCell = { position: Coordinate; cell: Cell }
type WordPosition = { startPos: Coordinate; isHorizonal: bool; length: int }

type Board private (cells: Map<Coordinate, Cell>, wordPositions: WordPosition list) =
    static let wordLength (row: int) (rowIsHorizontal: bool) (cellPositions: Coordinate list) (wordStartIndexInRow: int) : int =
        cellPositions 
        |> List.mapi (fun idxInRow cell -> idxInRow, cell) 
        |> List.tryFindBack (fun (idxInRow, cell) -> (if rowIsHorizontal then cell.x else cell.y) = idxInRow + wordStartIndexInRow)
        |> Option.map (fun (idxInRow, _) -> idxInRow + 1)
        |> Option.defaultValue (cellPositions.Length)
            
    static let findWordsInSingleRow (cellCoordinatesRow: int * Coordinate list) (rowIsHorizontal: bool) = 
        let rowIdx, cells = cellCoordinatesRow
        let wLength = wordLength rowIdx rowIsHorizontal

        let rec loop cells parsedWords =
            match cells with 
            | h1 :: h2 :: _ when (if rowIsHorizontal then (h2.x - h1.x) else (h2.y - h1.y)) = 1 -> 
                let length = wLength cells (if rowIsHorizontal then h1.x else h1.y)
                let wordPosition = { 
                    startPos = if rowIsHorizontal then { y = rowIdx; x = h1.x } else { y = h1.y; x = rowIdx }
                    isHorizonal = rowIsHorizontal
                    length = length
                }
                loop (cells |> List.skip length) (wordPosition :: parsedWords)
            | _ :: tail -> loop tail parsedWords
            | [] -> parsedWords

        loop cells []

    static let searchWordsInOneDirection (cellPositions: Coordinate list) (searchHorizontally: bool) =
        // sugrupuojam visas celles pagal viena is koordinaciu (y - pagal eilute, x - pagal stulpeli),
        // ir kiekviena grupe isrikiuojame kitos koordinates didejimo tvarka.
        cellPositions 
        |> List.groupBy (fun c -> if searchHorizontally then c.y else c.x)
        |> List.map (fun (i, list) -> i, list |> List.sortBy (fun c -> if searchHorizontally then c.x else c.y)) 
        |> List.collect (fun row -> findWordsInSingleRow row searchHorizontally) // row atstoja horizontalia arba vertikalia eile

    static let findAllWords (cellPositions: Coordinate list) = 
        let horizontalWordPositions = searchWordsInOneDirection cellPositions true
        let verticalWordPositions = searchWordsInOneDirection cellPositions false
        horizontalWordPositions @ verticalWordPositions

    let letterCanBePlacedInCell (letter: Letter, cell: Cell) =
        let typesMatch = letter.``type`` = cell.``type`` 
        let cellNotOccupied = cell.occupant.IsNone
        let cellOccupantMatchesLetter = cell.occupant |> Option.map (fun charOccupyingCell -> letter.ch = charOccupyingCell) |> Option.defaultValue (false)
        typesMatch && (cellNotOccupied || cellOccupantMatchesLetter)
        
    member private this.updateCells (word: Word) position =
        position 
        |> this.getCellsCoordinatesInPosition
        |> List.zip (word |> List.ofSeq)
        |> List.fold (fun cells (letter, pos) -> cells |> Map.add pos { ``type`` = letter.``type``; occupant = Some letter.ch }) (cells)
        |> fun updatedCells -> new Board(updatedCells, wordPositions)

    member this.wordFitsInPosition (word: Word) position : bool =
        if word.Length <> position.length then false
        else 
            let positionCells = this.getCellsInPosition position

            not (positionCells
                |> List.zip word
                |> List.map (letterCanBePlacedInCell)
                |> List.contains false)

    member this.getCellsCoordinatesInPosition position =
        let rec loop currentCoord left acc = 
            if left = 0 then acc |> List.rev
            else
                let { x = x; y = y } = currentCoord
                let nextCoord = if position.isHorizonal then { x = x + 1; y = y } else { x = x; y = y + 1 }
                loop nextCoord (left - 1) (nextCoord :: acc)
        
        loop position.startPos (position.length - 1) [ position.startPos ]

    member this.getCellsInPosition position: Cell list = 
        position |> this.getCellsCoordinatesInPosition |> List.map (fun coord -> cells.[coord])

    // jei sekmingai padeda zodi, grazina lenta, jei ne, negrazina nieko
    member this.placeWord (word: Word) position : Board option =
        let boardHasSuchPosition = wordPositions |> List.contains position 
        let wordFits = this.wordFitsInPosition word position

        if boardHasSuchPosition && wordFits then Some (this.updateCells word position)
        else None

    member this.cells = cells
    member this.wordPositions = wordPositions

    new (cells: PositionedCell list) = 
        let cellsAsMap = cells |> List.map (fun pc -> pc.position, pc.cell) |> Map.ofList
        let words = findAllWords (cells |> List.map (fun x -> x.position))
        new Board(cellsAsMap, words)

type Game private (board: Board, unusedWords: Word list, placedWords: Map<WordPosition, Word>) =
    let filterSingleWordFromUnused word = 
        let notYetFiltered = true;
        unusedWords |> List.fold (fun acc x -> if notYetFiltered && x = word then acc else x :: acc) []

    member this.placeWord word position : Game option =
        let gameHasSuchWord = unusedWords |> List.contains word
        let positionNotOccupied = not (placedWords.ContainsKey position)

        if gameHasSuchWord && positionNotOccupied then
            board.placeWord word position
            |> Option.map (fun updatedBoard ->
                Game (updatedBoard, 
                      filterSingleWordFromUnused word,
                      placedWords |> Map.add position word))
        else None

    member this.board = board
    member this.unusedWords = unusedWords
    member this.placedWords = placedWords

    new (board, words) = new Game (board, words, Map.empty)
        

