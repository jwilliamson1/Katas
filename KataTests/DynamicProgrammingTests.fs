module DynamicProgrammingTests

open NUnit.Framework
open FsUnit
open Microsoft.FSharp.Collections
open System
open System.Collections.Generic

let rec countStairs(input: int): int =
    match input with
    | 0 -> 0
    | 1 -> 1
    | 2 -> 2 + countStairs(1) + countStairs(1)
    | input when input > 1 -> 2
    | _ -> raise <| Exception("Oops")
        

[<TestCase(1, 1)>]
[<TestCase(2, 2)>]
let countStairsTest(input: int, expected: int) = 
    countStairs input
    |> should equal 1

