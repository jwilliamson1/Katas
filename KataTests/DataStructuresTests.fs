module DataStructuresTests
open NUnit.Framework
open FsUnit
open Microsoft.FSharp.Collections
open System
open System.Collections.Generic
open DataStructures

[<SetUp>]
let Setup () =
    ()

[<Test>]
let ``growing array list works`` () = 
    let lst = Lst(1) // 1
    lst.add 2 //2
    lst.add 3 //4
    lst.add 4 //4
    lst.add 5 // 8

    printf "here"