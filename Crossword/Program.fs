module Crossword =
    open System.IO
    open Crossword
    open System.Text.RegularExpressions
    open System.Collections.Generic
    open Crossword

    let addUpperCaseLetters (s: string) = s + s.ToUpperInvariant()
    let vowels = addUpperCaseLetters "aąeęėiįyouųū"
    let consonants = addUpperCaseLetters "bcčdfghjklmnprsštvzž"
    let boardSymbols = (addUpperCaseLetters "xo") + " "
    let allAllowedWordSymbols = vowels + consonants

    let matchChar ch regex = Regex.Match(ch.ToString(), "[" + regex + "]").Success
    let isAllowedWordSymbol (ch: char) = matchChar ch allAllowedWordSymbols
    let notAllowedWordSymbol (ch: char) = not (isAllowedWordSymbol ch)
    let isVowel (ch: char) = matchChar ch vowels
    let isConsonant (ch: char) = matchChar ch consonants
    let isAllowedBoardSymbol (ch: char) = matchChar ch boardSymbols
    let notAllowedBoardSymbol (ch: char) = not (isAllowedBoardSymbol ch)

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

        

    type Game = { board: Board; words: Word list }

    type OptFlatmapChain() =
        member this.Bind(m, f) = Option.bind f m
        member this.Return(x) = Some x

    let optFlatMapChain = new OptFlatmapChain()

    let getWords (lines: string list) = 
        match lines with
        | [] -> None
        | h :: _ -> Some (h.Split(' ') |> List.ofArray)

    let parseWords (words: string list): Word list option =
        match words |> List.collect (List.ofSeq) |> List.tryFind notAllowedWordSymbol with
        | Some _ -> None
        | None -> Some (words |> List.map (fun w -> w |> List.ofSeq |> List.map (Letter.create) |> List.choose id))  //priestai patikrinau kad nebutu illegal charu, tai cia galiu naudot choose

    let getBoardLines (lines: string list) =
        match lines with
        | _ :: _ :: boardLines -> Some boardLines
        | _ -> None

    let parseCell ch coord = 
        if ch = 'x' || ch = 'X' then Some { position = coord; ``type`` = Consonant }
        else if ch = 'o' || ch = 'O' then Some { position = coord; ``type`` = Vowel }
        else if ch = ' ' then Some { position = coord; ``type`` = Space }   // jei bent viena celle grazina None, tai visa lenta none, todel reikia sito
        else None

    let noneIfContainsNone (list: 'a option list) = if (list |> List.contains None) then None else Some (list |> List.choose id)

    let parseCells (line: string) (yCoord: int) =
        line 
        |> List.ofSeq 
        |> List.mapi (fun xCoord ch -> parseCell ch { x = xCoord; y = yCoord }) 
        |> noneIfContainsNone 
        |> Option.map (List.filter (fun x -> x.``type`` <> Space))

    let parseBoardCells (lines: string list) =
        let boardSize = lines.Length
        lines
        |> List.mapi (fun yCoord line -> 
            if (line.Length > boardSize) then None
            else parseCells line yCoord)
        |> noneIfContainsNone
        |> Option.map (fun t -> t |> List.collect id)

    let readData filename =
        if not (File.Exists(filename)) then None
        else
            let lines = File.ReadAllLines(filename) |> List.ofArray
           
            optFlatMapChain {
                let! words = lines |> getWords
                let! parsedWords = words |> parseWords
                let! boardLines = lines |> getBoardLines
                let! boardCells = boardLines |> parseBoardCells
                return { words = parsedWords; board = Board boardCells }
            }
            

            // same

            //match lines |> getWords |> Option.bind parseWords with 
            //| None -> None
            //| Some words ->
            //    match lines |> getBoardLines |> Option.bind (parseBoard) with
            //    | None -> None
            //    | Some board ->
            //        let x = board
            //        Some(x)

    let run =
        printfn "%A" (isAllowedWordSymbol 'Į')
        let data = readData "data.txt"
        System.Console.ReadKey() |> ignore

Crossword.run
