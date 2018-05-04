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
    
type Board private (cells: Map<Coordinate, SType>, words: PositionedWord list) =
    // returns word length and leftover cells
    static let wordLength (y: int) (xStart: int) (cells: Cell list): (int * Cell list) =
        let length =
            cells 
            |> List.mapi (fun x cell -> x, cell) 
            |> List.tryFindBack (fun (x, cell) -> (cell.position.x - xStart) = x)
            |> Option.map (fun (x, _) -> x + 1)
            |> Option.defaultValue (cells.Length)
            
        length, cells |> List.skip length

    static let findWordsInRow (cellRow: int * Cell list): PositionedWord list = 
        let y, cells = cellRow

        let rec loop cells parsedWords =
            match cells with 
            | head :: next :: _ when (next.position.x - head.position.x) = 1 -> 
                let length, leftoverCells = wordLength y head.position.x cells
                let positionedWord = { startPos = { y = y; x = head.position.x }; isHorizonal = true; length = length; occupant = None }
                loop leftoverCells (positionedWord :: parsedWords)
            | _ :: tail -> loop tail parsedWords
            | [] -> parsedWords

        loop cells []

    static let findHorizontalWords (cells: (int * Cell list) list): PositionedWord list =(* List.empty*)
        cells |> List.collect findWordsInRow
        
    static let findVerticalWords (cells: (int * Cell list) list): PositionedWord list = List.Empty

    static let findAllWords (cells: Cell list) = 
        let x = findHorizontalWords (cells |> List.sortBy (fun c -> c.position.y) |> List.groupBy (fun c -> c.position.y)) 
        let t = x
                @ findVerticalWords (cells |> List.sortBy (fun c -> c.position.x) |> List.groupBy (fun c -> c.position.x))
        t

    new (cells: Cell list) = 
        let cellsAsMap = cells |> List.map (fun cell -> cell.position, cell.``type``) |> Map.ofList
        let words = findAllWords cells
        new Board(cellsAsMap, words)

