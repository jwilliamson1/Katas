module DataStructures

open System

type Lst<'a> (element: 'a) =    
    let mutable arr = [|element|]
    let mutable kaboose = 0

    let appendAll (inputLst: Lst<'a>) =
        let rec internalInsertFrom counter =
            if counter = inputLst.len then
                ()
            else
                kaboose <- kaboose + 1
                arr[kaboose] <- inputLst.elementAt counter
                internalInsertFrom <| counter + 1                
        internalInsertFrom 0

    member _.elementAt index = 
        arr[index]
    
    member _.add element =
        match arr.Length - 1 with
        | l when kaboose < l -> 
             kaboose <- kaboose + 1
             arr[kaboose] <- element
        | l when kaboose = l ->
            let dest = Array.zeroCreate <| (l+1) * 2
            Array.Copy(arr, dest, arr.Length)
            arr <- dest
            kaboose <- kaboose + 1
            arr[kaboose] <- element
        | _ -> raise <| Exception("should not get here")

    member _.len = kaboose + 1

    member _.addRange (inputLst: Lst<'a>) =
        
        let rec calculateArraySize curArrLen incomingArrLen =
            if curArrLen < incomingArrLen
                then calculateArraySize (curArrLen * 2) incomingArrLen
            else curArrLen

        if inputLst.len = 0 then ()

        if inputLst.len  + arr.Length < kaboose
            then ()
            else 
            let reqLen = (inputLst.len + kaboose + 1)
            let newArraySize = calculateArraySize arr.Length reqLen
            let dest = Array.zeroCreate newArraySize
            Array.Copy(arr, dest, arr.Length)
            arr <- dest
            appendAll inputLst  