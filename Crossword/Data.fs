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