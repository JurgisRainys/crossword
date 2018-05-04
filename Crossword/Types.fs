module Types

open Helpers

type SType = Vowel | Consonant | Space // space yra, kad kai parsina duomenis, vietoj tarpu nepridetu kitu zenklu
type Letter = private { ``type``: SType; ch: char } with
    static member create (ch: char) = 
        if (isVowel ch) then Some { ``type`` = Vowel; ch = ch }
        else if (isConsonant ch) then Some { ``type`` = Vowel; ch = ch }
        else None

type Word = Letter list

type Coordinate = { x: int; y: int }
type Cell = { position: Coordinate; ``type``: SType}
type PositionedWord = { startPos: Coordinate; isHorizonal: bool; length: int; occupant: Word option }

let takeXCoord (r: Coordinate) = r.x
    
type Board private (cells: Map<Coordinate, SType>, words: PositionedWord list) =
    // returns word length and leftover cells
    static let wordLength (row: int) (horizontalRow: bool) (cellPositions: Coordinate list) (colStartIdx: int) : (int * Coordinate list) =
        let length =
            cellPositions 
            |> List.mapi (fun column cell -> column, cell) 
            |> List.tryFindBack (fun (column, cell) -> (if horizontalRow then cell.x else cell.y) = column + colStartIdx)
            |> Option.map (fun (column, _) -> column + 1)
            |> Option.defaultValue (cellPositions.Length)
            
        length, cellPositions |> List.skip length

    static let findWordsInSingleRow (cellCoordinatesRow: int * Coordinate list) (horizontalRow: bool) = 
        let rowIdx, cells = cellCoordinatesRow
        let wLength = wordLength rowIdx horizontalRow

        let rec loop cells parsedWords =
            match cells with 
            | h1 :: h2 :: _ when (if horizontalRow then (h2.x - h1.x) else (h2.y - h1.y)) = 1 -> 
                let length, leftoverCellsInRow = wLength cells (if horizontalRow then h1.x else h1.y)
                let positionedWord = { 
                    startPos = if horizontalRow then { y = rowIdx; x = h1.x } else { y = h1.y; x = rowIdx }; 
                    isHorizonal = true; 
                    length = length; 
                    occupant = None 
                }
                loop leftoverCellsInRow (positionedWord :: parsedWords)
            | _ :: tail -> loop tail parsedWords
            | [] -> parsedWords

        loop cells []

    static let findHorizontalWords (cells: (int * Coordinate list) list): PositionedWord list =
        cells |> List.collect (fun row -> findWordsInSingleRow row true)
        
    static let findVerticalWords (cells: (int * Coordinate list) list): PositionedWord list = 
        cells |> List.collect (fun row -> findWordsInSingleRow row false)

    static let findAllWords (cellPositions: Coordinate list) = 
        let horizontalWords = findHorizontalWords (cellPositions |> List.sortBy (fun c -> c.y) |> List.groupBy (fun c -> c.y))
        let verticalWords = findVerticalWords (cellPositions |> List.sortBy (fun c -> c.x) |> List.groupBy (fun c -> c.x))
        let t = horizontalWords @ verticalWords
        t

    new (cells: Cell list) = 
        let cellsAsMap = cells |> List.map (fun cell -> cell.position, cell.``type``) |> Map.ofList
        let words = findAllWords (cells |> List.map (fun x -> x.position))
        new Board(cellsAsMap, words)

type Game = { board: Board; words: Word list }

