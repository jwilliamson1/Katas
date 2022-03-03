module KataTests

open NUnit.Framework
open FsUnit
open Microsoft.FSharp.Collections

[<SetUp>]
let Setup () =
    ()

let stringIsPermutation (s1: string) (s2: string): bool =
    match (s1, s2) with
    | (_, null) -> false
    | (null, _) -> false
    | (s1, s2) when s1.Length <> s2.Length -> false
    | _ -> Array.sort(s1.ToCharArray()) = Array.sort(s2.ToCharArray())

[<Test>]
[<TestCase("a", "")>]
[<TestCase("", "b")>]
[<TestCase("a", "ab")>]
[<TestCase("as", "b")>]
let ``unequal string lengths should be false`` (s1, s2) =
    stringIsPermutation s1 s2 |> should be False

[<Test>]
[<TestCase("a", null)>]
[<TestCase(null, "b")>]
let ``either string null should be false`` (s1, s2) =
    stringIsPermutation s1 s2 |> should be False

[<Test>]
[<TestCase("abc", "bac")>]
[<TestCase("xyz", "zyx")>]
let ``permutations should be true`` (s1, s2) =
    stringIsPermutation s1 s2 |> should be True

[<Test>]
[<TestCase("abc", "bzc")>]
[<TestCase("xbc", "bzc")>]
let ``non-permutations should be false`` (s1, s2) =
    stringIsPermutation s1 s2 |> should be False