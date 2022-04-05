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

    let lst2 = Lst(6) // 1
    lst2.add 7 //2
    lst2.add 8 //4
    lst2.add 9 //4
    lst2.add 10 // 8

    lst.addRange lst

    printf "here"