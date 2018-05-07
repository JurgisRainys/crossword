module Data 

open System.IO
open Types
open Helpers

let getWords (lines: string list) = 
    match lines with
    | [] -> None
    | h :: _ -> Some (h.Split(' ') |> List.ofArray)

let parseWords (words: string list): Word list option =
    match words |> List.collect (List.ofSeq) |> List.tryFind notAllowedWordSymbol with
    | Some _ -> None
    | None -> Some (
        words 
        |> List.map (fun w -> 
            w.ToUpperInvariant() 
            |> List.ofSeq 
            |> List.map (Letter.create) 
            |> List.choose id
        )
    )  //priestai patikrinau kad nebutu illegal charu, tai cia galiu naudot choose

let getBoardLines (lines: string list) =
    match lines with
    | _ :: _ :: boardLines -> Some boardLines
    | _ -> None

let parseCell ch coord = 
    if ch = 'x' || ch = 'X' then Right (Some { ``type`` = Consonant; occupant = None })
    else if ch = 'o' || ch = 'O' then Right (Some { ``type`` = Vowel;  occupant = None })
    else if ch = ' ' then Right None // jei bent viena celle grazina None, tai visa lenta none, todel reikia sito
    else Left ()

let noneIfContainsNone (list: 'a option list) = if (list |> List.contains None) then None else Some (list |> List.choose id)

let parseCells (line: string) (yCoord: int) =
    Some (
        line 
        |> List.ofSeq 
        |> List.mapi (fun xCoord ch -> 
            let coord = { x = xCoord; y = yCoord }
            let res = parseCell ch coord 
            coord, res
        ) 
        |> List.map (fun (coord, resultEither) -> coord, resultEither |> Either.noneIfLeft)
        |> List.choose (fun (coord, resultOpt) -> resultOpt |> Option.map (fun result -> coord, result))
        |> List.map (fun (coord, result) -> { position = coord; cell = result })
    )

let parseBoardCells (lines: string list) =
    let boardSize = (lines |> List.maxBy(fun x -> x.Length)).Length
    lines
    |> List.mapi (fun yCoord line -> 
        if (line.Length > boardSize) then None
        else parseCells line yCoord)
    |> noneIfContainsNone
    |> Option.map (fun t -> t |> List.collect id)

let parseGameFrom filename =
    if not (File.Exists(filename)) then None
    else
        let lines = File.ReadAllLines(filename) |> List.ofArray
           
        optFlatMapChain {
            let! words = lines |> getWords
            let! parsedWords = words |> parseWords
            let! boardLines = lines |> getBoardLines
            let! boardCells = boardLines |> parseBoardCells
            return Game (Board boardCells, parsedWords)
        }

let getMaxAxisValue (coordinates: Coordinate list) (findMaxOfX: bool) : int =
    coordinates |> List.map (fun { x = x; y = y } -> if findMaxOfX then x else y) |> List.max

let getBoardCellCoordinates (board: Board) =
    board.cells |> Map.toList |> List.map (fun (coord, _) -> coord)

let printResults (game: Game) =
    let getMaxAxisValue = game.board |> getBoardCellCoordinates |> getMaxAxisValue

    let maxX = getMaxAxisValue true
    let maxY = getMaxAxisValue false

    let mutable s = ""
    
    let digitCount num = num.ToString().Length
    let linesForXNumbers = digitCount maxX
    let maxYDigitCount = digitCount maxY

    let nums  = [0 .. maxX] |> List.map (fun x -> 
        let numAsString = x.ToString()
        let zerosToAdd = List.init (linesForXNumbers - numAsString.Length) (fun _ -> '0') 
                        |> List.fold (fun acc ch -> acc + ch.ToString()) ""
        (numAsString + zerosToAdd).ToCharArray())

    [0.. (nums.[0].Length - 1)] |> List.iter (fun j ->
        s <- s + "   " 
        [0 .. maxYDigitCount] |> List.iter (fun _ -> s <- s + " ")
        [0 .. (nums.Length - 1)] |>  List.iter (fun i -> s <- s + nums.[i].[j].ToString() + " ")
        s <- s + "\n")

    s <- s + "\n"
    for y = 0 to maxY do
        let extraSpoces = 
            [0 .. (maxYDigitCount - (digitCount y))] 
            |> List.map (fun x -> x.ToString()) 
            |> List.fold (fun acc _ -> " " + acc ) ("")
            
        s <- s + y.ToString() + extraSpoces + "   "
        for x = 0 to maxX do
            if game.board.cells.ContainsKey { x = x; y = y } then
                let cellOccupant = game.board.cells.[{ x = x; y = y }].occupant
                match cellOccupant with
                | None -> s <- s + "_ "
                | Some ch -> s <- s + ch.ToString() + " "
            else s <- s + "  "
        s <- s + "\n"
                
    printfn "%s" s



    