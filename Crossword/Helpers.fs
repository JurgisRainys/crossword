module Helpers

open System.Text.RegularExpressions

let addUpperCaseLetters (s: string) = s + s.ToUpperInvariant()
let vowels = addUpperCaseLetters "aąeęėiįyouųū"
let consonants = addUpperCaseLetters "bcčdfghjklmnprsštvzž"
let boardSymbols = (addUpperCaseLetters "xo") + " "
let allAllowedWordSymbols = vowels + consonants

let matchChar ch (allowed: string) = allowed.Contains(ch.ToString())
let isVowel (ch: char) = matchChar ch vowels
let isConsonant (ch: char) = matchChar ch consonants
let isSpace (ch: char) = ch = ' '
let notAllowedWordSymbol (ch: char) = not (matchChar ch allAllowedWordSymbols)
let notAllowedBoardSymbol (ch: char) = not (matchChar ch boardSymbols)

type OptFlatmapChain() =
    member this.Bind(m, f) = Option.bind f m
    member this.Return(x) = Some x

let optFlatMapChain = new OptFlatmapChain()

[<AutoOpen>]
module Either =
    type Either<'a, 'b> =
        | Left of 'a
        | Right of 'b

    let isLeft = function
      | Left _ -> true
      | _      -> false

    let isRight = function
      | Right _ -> true
      | _      -> false

    let noneIfLeft (either: Either<'a, 'b option>) = 
        match either with
        | Right cell -> cell
        | _ -> None