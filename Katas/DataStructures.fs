module DataStructures

open System

type Lst<'a> (element: 'a) =    
    let mutable arr = [|element|]
    let mutable kaboose = 0
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
