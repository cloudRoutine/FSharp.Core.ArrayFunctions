namespace FSharp.Core.ArrayFunctions



[<RequireQualifiedAccess>]
module Array2D =

    


    let flatten (arr2D:'a[,]) : 'a [] =
        let arr = Array.zeroCreate  arr2D.Length
        let rec loop x y acc =
            if   x = arr2D.GetLength(0) then arr
            elif y = arr2D.GetLength(1) then loop (x+1) 0 (acc+1) 
            else arr.[acc] <- arr2D.[x,y] 
                 loop x (y+1) (acc+1)
        loop 0 0 0

    


    ()