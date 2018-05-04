module Helpers

open System.Text.RegularExpressions

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

type OptFlatmapChain() =
    member this.Bind(m, f) = Option.bind f m
    member this.Return(x) = Some x

let optFlatMapChain = new OptFlatmapChain()