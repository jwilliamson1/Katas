module StringsTests

open NUnit.Framework
open FsUnit
open Microsoft.FSharp.Collections
open System
open System.Collections.Generic

[<SetUp>]
let Setup () =
    ()

let bigString = 
    "SvMvMFGcJZONhVcPmZhi
dxVFfriAWziUEvDBzuMG
nXlpBfJULjKUnAhmqTJD
SLJqatQDNBPePgDpkLxU
vKxVaNsDIYSqvmelHEsN
CBZxBNXxJcxrJuUngMqj
mdUcNbNgxlRGrNQlhksV
yJhtmqrVXAjliGWlzRPY
GGwwZufNsvkajSVlBNHd
wsXUsFvXlvtXTQLKjQkO
AEPioEKRDCgYysMaUles
rPHZhZAeMxcFOaalCzHF
NgrcnAYWrmVakasDiSKh
cGrPBaclfoGTzHoZetZW
VCOsjQaIUzQsbprCYBuo
PHohVZGKxBQSodtVGBVf
BgtAwxlPeGnSFwLTAppy
FITIkxSwmMFJBAsCehBE
FrkYCLnRxyTGzwEOYrFH
SplVtijFBLApqNdRZgzO
sgtTmNJwbFATFZUyPDBl
AosaGMqGcFGNrvRrfuGE
NLtdPwwFQgoAIsLkEQha
ODfVDAnGvJFJJkUiBKkk
NoomHrWuTuGraQUrAsgk
MgWAKQqVwkcjCgFBXbgy
UsfpTAzMebipldlVxYtB
aBXrENYSIWNGffsKxIWL
UwRUYUBrOLbDrZwLrbDO
imxjetimaaObhqGOPZPG
zYOnhGHsTQzYXflofUus
LjQLTpowkMxlQeEcGFpe
sGPGAZSuURZhwNxpWwLC
VjpAXIhjGGdUWPCWJmlg
SwixSGBHuEYiegmxPHVY
fIxryspPXrtsRtGRIyWQ
mDhyPtifJaOPtrhfvnMG
jNCEnmEgejempeuVvrFj
DLEbdYBuQAbajliyAliY
zBfzkyemNxQSSBMYouOw
QwlWqJStDYbafZncfQrG
EOaPMXwgwQswHirwyVbh
vTryAzSWRRloDRlYicyg
RsPzLZWbHlPCXryduxlb
YxgZjnfWNvIKRTaOEHrw
dhjmBSsGwxPoPzGEnIoP
CppQGzdMVWaKfpgftooV
aRokfnVrDgNzbWvHWpDo
stHFeScOJQnGDdGcZiAF
uWKprWOChehdOeGGQLME
nhZGzfnVyvVcIUKHjPPM
MCoZybmXeYKdLjJQMRuL
OsStwFksyKPvZKIVFsGT
IRmcRLMRnGoClRUtlNjm
KmMVhjGtDDmxKfwjyHMG
HlRXWTlRVbOaIfVzUnlx
fjGpCfmxDFElYLVNgVhf
hKOiHJisqrkwxjLUiTwG
dZMLXmZStcYYPiaNSNoK
cYaLnNtHuNwIfzOZXtrP
krZbhXFJQefumwiFiYeU
LYlJGqsCmUxvBGtSiHrI
pgqrFedtILybbXGICOng
GgdEYerUeSzJAOLDMRed
aFtaCDImZyOUGExsjMMj
ojoEZoBHdcGnGiVfBqPz
LoNtJKboNRZSokRXeqIX
kWxzerBkyPATqPsnhXum
ZwIkooOiFeOJvDtWZpOO
sIMTGqHKOaYEpCanBKTs
MgeeaqSuujAckFzjuJZW
mPZoBLsALqdbIUhqZuZo
ZpjuOgLQckrSTbMHEyQm
onOIomJZnqQRMjRCizSQ
YPuBOVUHjKHHrFbpCOix
MgYezaepspDyMFXFwivK
cuJlYurDtVcnCwZJLWWP
hfBLWxZnubaXpkdkXthJ
cahMYYTOJQdZLDvEcVuI
GVpsXwAmaTNPakBdAzDQ
dOlqsQtrNwMBEcWVCSJI
iUkmVprDGVyRdWbEOvxB
LFbdXeuBrkoTepmVJLUD
ZBVKlGUpbjjGcOVlImFZ
maSzIdoyHBuprMtpWvIp
WcUFvKHdkKVjRgioDTot
zsyhFHLOlqPTYExhRTHx
EykLXhdAjFIiSEUCEkQG
ytePQJakHnMgceNqWmcm
mpDXudupPhTAdFpXLevn
ADfMBZBvithfWJQyjGej
odYbGqdsaIGspxUtqOtY
InFaDpUIfkAVFzqamPWs
CLHouTApknrzyOQdBKtD
RkLYmpiqvYgVmEWGIvss
wypafjDMWCaRiIZdkZTG
qlBGhlMfDVjktPgrjOFb
eSptxFkbbdmBCIwCDcaR
dPXggEhjbjWwaGjMfLeM
GzOYOQzkmOzFawWbKLKg"

let stringIsPermutation (s1: string) (s2: string): bool =

    let detectPermutation (s1: string, s2: string): bool =
        let charHisto: bool[] = Array.zeroCreate <| int Char.MaxValue
        let rec detectPermutationInternal (pos: int): bool =
            if pos = s1.Length
                then charHisto |> Array.forall(fun b -> b = false)
                else
                    let ch1 = int s1[pos]
                    let ch2 = int s2[pos]
                    charHisto[ch1] <- not charHisto[ch1]
                    charHisto[ch2] <- not charHisto[ch2]
                    detectPermutationInternal(pos + 1)
        detectPermutationInternal 0

    match (s1, s2) with
    | (_, null) -> false
    | (null, _) -> false
    | (s1, s2) when s1.Length <> s2.Length -> false
    | inputs -> detectPermutation inputs

let stringIsPermutationSort (s1: string) (s2: string): bool =


    match (s1, s2) with
    | (_, null) -> false
    | (null, _) -> false
    | (s1, s2) when s1.Length <> s2.Length -> false
    | _ -> Array.sort(s1.ToCharArray()) = Array.sort(s2.ToCharArray()) // n log n

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

[<Test>]
let ``array balancing is faster`` () =
    let bigStringSorted = Array.sort <| bigString.ToString().ToCharArray()
    let bigString2 = new string (bigStringSorted)
    stringIsPermutation bigString bigString2 |> should be True

[<Test>]
let ``sorting is slower`` () =
    let sorted = Array.sort <| bigString.ToString().ToCharArray() 
    let bigString2 = new string (sorted)
    stringIsPermutationSort bigString bigString2 |> should be True