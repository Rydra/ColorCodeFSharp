module DomainTests

open NUnit.Framework
open ColorCodeSharp.Domain
open Chessie.ErrorHandling
open Swensen.Unquote
open System

[<Test>]
let ``Change color test``() =
    toColor "Yellow" |> returnOrFail =! Yellow
    toColor "red" |> returnOrFail =! Red
    toColor "blUe" |> returnOrFail =! Blue
    toColor "pink" |> returnOrFail =! Pink
    toColor "blaCK" |> returnOrFail =! Black

[<TestCase("Pink, Red, Black, Green", 4, 0)>]
[<TestCase("Pink, Red, Black, Green", 4, 0)>]
[<TestCase("Pink, Red, Black, Green", 4, 0)>]
[<TestCase("Pink, Red, Black, Green", 4, 0)>]
[<TestCase("Pink, Red, Black, Green", 4, 0)>]
let ``Checker tests`` (colorsText:string) matchpos matchcolor =
    let colors = 
        colorsText.Split([|","|], StringSplitOptions.RemoveEmptyEntries) 
        |> Array.map (fun s -> s.Trim() |> toColor |> returnOrFail)
        |> List.ofArray
    let solution = [ Pink; Red; Black; Green ] // A solution
    let feedback = colorChecker colors solution
    feedback.MatchedColors =! matchcolor
    feedback.MatchedPositions =! matchpos